method Testing() {
  // Add assertions to check max here.
  var t := {"OP_OR", "OP_AND", "OP_REL", "OP_ADIT", "OP_MULT", "OP_NOT", "NUM", "PAR_ABR", "PAR_CER","ID"};
  var v := {"expr", "exprp", "eand", "eandp", "erel", "erelp", "arit", "aritp", "term", "termp", "fact", "rando"};
  var r := map[ "expr"  := [["eand", "exprp"]],
				"exprp" := [["lambda"],["OP_OR", "eand", "exprp"]],
				"eand"  := [["erel", "eandp"]],
				"eandp" := [["lambda"],["OP_AND", "erel", "eandp"]],
				"erel"  := [["arit", "erelp"]],
				"erelp" := [["lambda"],["OP_REL", "arit", "erelp"]],
				"arit"  := [["term", "aritp"],
				"aritp" := [["lambda"],["OP_ADIT", "term", "aritp"]],
				"term"  := [["fact", "termp"]],
				"termp" := [["lambda"],["OP_MULT", "fact", "termp"]],
				"fact"  := [["OP_NOT", "fact"],["OP_ADIT", "fact"],["rando"]],
				"rando" := [["NUM"],["ID"],["PAR_ABR", "expr", "PAR_CER"]]];
  assert ("ID" in t);
}