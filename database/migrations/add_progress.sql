ALTER TABLE stats ADD COLUMN progress TEXT;
ALTER TABLE statsguest ADD COLUMN progress TEXT;

UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 63;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 219;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 455;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 765;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 1143;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 1587;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 2094;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 2664;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 3293;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD", "GLASS"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 3981;
UPDATE stats SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD", "GLASS", "MYRIAD"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 4726;

UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 63;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 219;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 455;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 765;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 1143;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 1587;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 2094;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 2664;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 3293;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD", "GLASS"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 3981;
UPDATE statsguest SET progress='{"unlocks":["BLAZE","HEAVEN","SHROOM","TIDE", "MIRROR", "ALCHEMY", "EMPTY", "DUALITY", "SEER", "FEVER", "MORPH", "BLOOD", "GLASS", "MYRIAD"],"events":[{"contents":0,"tag":"EventTutorial"},{"contents":1,"tag":"EventTutorial"}]}' WHERE experience >= 4726;

