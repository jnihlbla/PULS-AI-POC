000010*** EDIT ALLOWED                                                          
000100*                      *** FÖRRÅDSDATAKRAVTABELL            ***           
000200 01  W4FDKRA2C0.                                                          
000300*                      *** FÖRRÅDSDATAKRAVTEXTER            ***           
000400     03 K01 PIC X(35) VALUE                                               
000500        'PALLET & FRAME                     '.                            
000600     03 K02 PIC X(35) VALUE                                               
000700        'PALLET & FRAME                     '.                            
000800     03 K03 PIC X(35) VALUE                                               
000900        'CORRUG.BOARD BOX (USA)             '.                            
001000     03 K04 PIC X(35) VALUE                                               
001100        '                                   '.                            
001200     03 K05 PIC X(35) VALUE                                               
001300        'WEGWERPVERPAKKING                  '.                            
001400     03 K06 PIC X(35) VALUE                                               
001500        'COR.BOARD CONT+SPACERS             '.                            
001600     03 K07 PIC X(35) VALUE                                               
001700        'PLYWOOD BOX                        '.                            
001800     03 K08 PIC X(35) VALUE                                               
001900        'PLYW.BOX+SPACERS                   '.                            
002000     03 K09 PIC X(35) VALUE                                               
002100        'PLYW.BOX MAX300KG                  '.                            
002200     03 K10 PIC X(35) VALUE                                               
002300        'CORRUG.BOARD BOX                   '.                            
002400     03 K11 PIC X(35) VALUE                                               
002500        'COR.BOARD BOX 300 KG               '.                            
002600     03 K12 PIC X(35) VALUE                                               
002700        'CORRUG.BOARD CONT                  '.                            
002800     03 K13 PIC X(35) VALUE                                               
002900        'CORRUG.BOARD BOX (NOT USA)         '.                            
003000     03 K14 PIC X(35) VALUE                                               
003100        'LIMITED HEIGHT MAX 75 CM          '.                             
003200     03 K15 PIC X(35) VALUE                                               
003300        '                                  '.                             
003400     03 K16 PIC X(35) VALUE                                               
003500        'VOLVO POOL VERPAKKING              '.                            
003600     03 K17 PIC X(35) VALUE                                               
003700        'CORRUG.BOARD BOX (CAR)             '.                            
003800     03 K18 PIC X(35) VALUE                                               
003900        'PLYW. BOX + AL-BAG                '.                             
004000                                                                          
004100 01  FILLER  REDEFINES W4FDKRA2C0.                                        
004200     03  BEFDKRAV-C2  OCCURS 18  PIC X(35).                               
004300*                      *** FÖRRÅDSDATAKRAVTEXT              ***           
004400*** END COPY W4FDKRA2C0  LENGTH=      OLD LENGTH=                         
