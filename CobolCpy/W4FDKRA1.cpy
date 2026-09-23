000010*** EDIT ALLOWED                                                          
000100*                      *** FÖRRÅDSDATAKRAVTABELL           ****           
000200 01  W4FDKRA1C0.                                                          
000300*                      *** FÖRRÅDSDATAKRAVTEXTER           ****           
000400     03 F01 PIC X(40) VALUE                                               
000500        'PALL OCH RAM        L,F,G,H,701(702,706)'.                       
000600     03 F02 PIC X(40) VALUE                                               
000700        'PALL OCH RAM        L,F,G,H,701(702,706)'.                       
000800     03 F03 PIC X(40) VALUE                                               
000900        'WELLÅDA CONT         0422,2610,2647,5500'.                       
001000     03 F04 PIC X(40) VALUE                                               
001100        '                                        '.                       
001200     03 F05 PIC X(40) VALUE                                               
001300        'PK (BIL-FLYG-CONT)   0422,2610,2647,5500'.                       
001400     03 F06 PIC X(40) VALUE                                               
001500        '                                        '.                       
001600     03 F07 PIC X(40) VALUE                                               
001700        'PLY-LÅD(BIL-FL)      1152,1153,1156,1658'.                       
001800     03 F08 PIC X(40) VALUE                                               
001900        '                                        '.                       
002000     03 F09 PIC X(40) VALUE                                               
002100        'PLY-LÅDA MAX 300KG   1152,1153,1156,1658'.                       
002200     03 F10 PIC X(40) VALUE                                               
002300        'WELLÅDA FLYG (IGLO)  3590,3591          '.                       
002400     03 F11 PIC X(40) VALUE                                               
002500        'PK MAX 300KG         0422,2610,2647,5500'.                       
002600     03 F12 PIC X(40) VALUE                                               
002700        'PK (BIL-FLYG)        0422,2610,2647,5500'.                       
002800     03 F13 PIC X(40) VALUE                                               
002900        '                                        '.                       
003000     03 F14 PIC X(40) VALUE                                               
003100        'BEGR.HÖJD:75 CM FLYG 0422,2610          '.                       
003200     03 F15 PIC X(40) VALUE                                               
003300        'PK (CONT)            5083               '.                       
003400     03 F16 PIC X(40) VALUE                                               
003500        'PALL OCH RAM        L,F,G,H,701(702,706)'.                       
003600     03 F17 PIC X(40) VALUE                                               
003700        'PK (BIL)             0422,2610,2647,5500'.                       
003800     03 F18 PIC X(40) VALUE                                               
003900        'PLYWL. + AL-PÅSE  1152,1153,1156+1152393'.                       
004000     03 F19 PIC X(40) VALUE                                               
004100        'PLYWL. + WELLÅDA  1628+1515,1516        '.                       
004200     03 F20 PIC X(40) VALUE                                               
004300        'PK + PLASTPÅSE    0422,5500+5925319     '.                       
004400                                                                          
004500 01  FILLER  REDEFINES W4FDKRA1C0.                                        
004600     03  BEFDKRAV-C1  OCCURS 20  PIC X(40).                               
004700*                      *** FÖRRÅDSDATAKRAVTEXT              ***           
004800*** END COPY W4FDKRA1C0  LENGTH=      OLD LENGTH=                         
