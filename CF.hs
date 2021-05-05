module CF where
import ClassFormat 
import TypedAST
import ConstPool


buildClassFile :: Class -> ClassFile
buildClassFile (Class modifier typ fields methods) =
	let
		constPool = buildConstPool(Class modifier typ fields methods)
		constPoolCount = getSize(constPool)
		fieldInfos = transformFields(constPool, fields)
		fiCount = getSize(fieldInfos)
		methodInfos = transformMethod(constPool, methods)
		methodCount = getSize(methodInfos)
	in 
		ClassFile Magic (MinorVersion(0)) (MajorVersion(60)) constPoolCount constPool (AccessFlags []) (ThisClass(0)) (SuperClass(0)) 0 [] fiCount fieldInfos methodCount methodInfos 0 []


getSize ::[a] -> Int
getSize [] = 0
getSize (_:tail) = 1 + getSize (tail)


transformFields :: ([CP_Info], [Typed Field]) -> [Field_Info]
transformFields(_, []) = []
transformFields(constPool, (c:cs)) =
	let 
		info = buildFieldInfo(constPool, c)
		rest = transformFields(constPool, cs)
	in 
		(info: rest)



buildFieldInfo :: ([CP_Info], Typed Field) -> Field_Info
buildFieldInfo(constPool, (Typed _ (Field modifier typ name))) =
	let 
		id = findInCp(constPool, name)
		typeId = findInCp(constPool, typ)
	in 
		Field_Info (AccessFlags []) id typeId 0 []


findInCp :: ([CP_Info], String) -> Int
findInCp (x,y) = 0


transformMethod :: ([CP_Info], [Typed Method]) -> [Method_Info]
transformMethod(_, []) = []
transformMethod(constPool, (c:cs)) = 
	let
		info = buildMethodInfo(constPool, c)
		rest = transformMethod(constPool, cs)
	in 
		(info: rest)


buildMethodInfo :: ([CP_Info], Typed Method) -> Method_Info
buildMethodInfo(constPool, (Typed _ (Method modifier typ name x statment))) =
	let
		id = findInCp(constPool, name)
		typeId = findInCp(constPool, typ)
	in 
		Method_Info (AccessFlags []) id typeId 1 []



