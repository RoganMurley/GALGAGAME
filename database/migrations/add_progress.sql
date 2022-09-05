ALTER TABLE stats ADD COLUMN progress TEXT;
ALTER TABLE statsguest ADD COLUMN progress TEXT;

UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 63;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 219;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 455;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 765;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1143;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1587;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2094;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2664;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3293;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD", "GLASS"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3981;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD", "GLASS", "MYRIAD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 4726;

UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 63;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 219;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 455;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 765;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1143;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1587;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2094;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2664;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3293;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD", "GLASS"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3981;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "VOID", "DUALITY", "EYE", "FEVER", "MORPH", "BLOOD", "GLASS", "MYRIAD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 4726;
