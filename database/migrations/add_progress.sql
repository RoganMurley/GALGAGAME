ALTER TABLE stats ADD COLUMN progress TEXT;
ALTER TABLE statsguest ADD COLUMN progress TEXT;

UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 63;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 219;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 455;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 765;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1143;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1587;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2094;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2664;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3293;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD", "GLASS"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3981;
UPDATE stats SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD", "GLASS", "PLASTIC"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 4726;

UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 63;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 219;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 455;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 765;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1143;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 1587;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2094;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 2664;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3293;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD", "GLASS"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 3981;
UPDATE statsguest SET progress='{"unlocks":["FIRE","SKY","SHROOM","WATER", "MIRROR", "GOLD", "VOID", "DUAL", "EYE", "FEVER", "CLAY", "BLOOD", "GLASS", "PLASTIC"],"events":["tutorial-0", "tutorial-1", "tutorial-complete"]}' WHERE experience >= 4726;
