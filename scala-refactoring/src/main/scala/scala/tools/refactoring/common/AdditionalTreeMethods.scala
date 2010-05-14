package scala.tools.refactoring.common

import tools.nsc.symtab.Flags
import reflect.ClassManifest.fromClass

trait AdditionalTreeMethods {
  
  this: CustomTrees =>

  import global._
  
  def cuRoot(p: global.Position): Option[Tree]
  def children(t: Tree): List[Tree]
  
  implicit def additionalValMethods(t: ValDef) = new {
    def needsKeyword = 
      !t.mods.hasFlag(Flags.PARAM) && 
      !t.mods.hasFlag(Flags.PARAMACCESSOR) && 
      !t.mods.hasFlag(Flags.CASEACCESSOR) && 
      !t.mods.hasFlag(Flags.SYNTHETIC) && 
      !t.symbol.isSynthetic
  }
  
  implicit def additionalTemplateMethods(t: Template) = new {
    def constructorParameters = t.body.filter {
      case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
      case _ => false
    }
    
    def primaryConstructor = t.body.filter {
      case t: DefDef => t.symbol.isPrimaryConstructor
      case _ => false
    }
    
    def earlyDefs = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats filter treeInfo.isEarlyDef
      case t @ DefDef(_, _, _, _, _, rhs)        if t.symbol.isConstructor && treeInfo.isEarlyDef(rhs) => rhs :: Nil
    } flatten
    
    def superConstructorParameters = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats collect {
        case Apply(Super(_, _), args) => args
      } flatten
    } flatten
  }  
  
  
  /**
   * Add some methods to Tree that make it easier to compare
   * Trees by position and to extract the position of a tree's
   * name, which is tricky for Selects.
   * */
  implicit def additionalTreeMethodsForPositions(t: Tree) = new {
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && t.pos.source == p.source
    def samePos(o: Tree)    : Boolean = samePos(o.pos)
    def sameTree(o: Tree)   : Boolean = samePos(o.pos) && fromClass(o.getClass).equals(fromClass(t.getClass))
    def namePosition(): Position = t match {
      case t: ModuleDef   => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t: ClassDef    => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t: TypeDef    => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t: ValOrDefDef =>
        
        val name = t.name.toString.trim
        
        /* In general, the position of the name starts from t.pos.point and is as long as the trimmed name.
         * But if we have a val in a function: 
         *   ((parameter: Int) => ..)
         *     ^^^^^^^^^^^^^^
         * then the position of the name starts from t.pos.start. To fix this, we extract the source code and
         * check where the parameter actually starts.
         * */
        lazy val src = t.pos.source.content.slice(t.pos.start, t.pos.point).mkString("")
        
        val pos = if(t.pos.point - t.pos.start == name.length && src == name) 
          t.pos withEnd t.pos.point
        else 
          t.pos withStart t.pos.point withEnd (t.pos.point + name.length)
        
        if(t.mods.isSynthetic && t.pos.isTransparent) 
          pos.makeTransparent
        else
          pos
          
      case t @ Select(qualifier, selector) => 
      
        if (qualifier.pos.isRange && qualifier.pos.start > t.pos.start) /* e.g. !true */ {
          t.pos withEnd qualifier.pos.start
        } else if (qualifier.pos.isRange && t.symbol != NoSymbol) {
          t.pos withStart (t.pos.end - t.symbol.nameString.length)
        } else if (qualifier.pos.isRange) {
          t.pos withStart (t.pos.point.max(qualifier.pos.end + 1))
        } else if (qualifier.pos == NoPosition) {
          t.pos
        } else {
          throw new Exception("Unreachable")
        }
        
      case _ => throw new Exception("uhoh")
    }
  }
  
  implicit def additionalTreeMethodsForIdentName(t: Ident) = new {
    def nameString = {
      if(t.name.toString == "<empty>")
        ""
      else if (t.symbol.isSynthetic && t.name.toString.contains("$"))
        "_"
      else if (t.symbol.isSynthetic)
        ""
      else t.name.toString
    }
  }
  
  /**
   * Make a Tree aware of its parent and siblings. Note
   * that these are expensive operations because they
   * traverse the whole compilation unit.
   * */
  implicit def additionalTreeMethodsForFamily(t: Tree) = new {
    def originalParent = cuRoot(t.pos) flatMap { root =>
    
      def find(root: Tree): Option[Tree] = {
        val cs = children(root)
        
        if(cs.exists(_ sameTree t))
          Some(root)
        else
          cs.flatMap(find).lastOption
      }
      find(root)
    }
    
    def originalLeftSibling  = findSibling(originalParent, 1, 0)
    
    def originalRightSibling = findSibling(originalParent, 0, 1)
    
    private def findSibling(parent: Option[Tree], compareIndex: Int, returnIndex: Int) = parent flatMap 
      (children(_) filter (_.pos.isRange) sliding 2 find (_ lift compareIndex map (_ samePos t) getOrElse false) flatMap (_ lift returnIndex))
  }
}