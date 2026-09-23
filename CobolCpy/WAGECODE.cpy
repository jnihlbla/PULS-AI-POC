000010*** EDIT ALLOWED                                                          
000100 01  WAGECODE.                                                            
000200*                                TABELL FÖR ATT RÄKNA FRAM                
000300*                                AGE-CODE                                 
000400*                                OBSERVERA !!!                            
000500*                                OCCURS MÅSTE VARA RÄTT !                 
000600     03  TABELL1.                                                         
000700         05  FILLER              PIC X(7) VALUE '03300 C'.                
000800         05  FILLER              PIC X(7) VALUE '04400 C'.                
000900         05  FILLER              PIC X(7) VALUE '04600 C'.                
001000         05  FILLER              PIC X(7) VALUE '05500 D'.                
001100         05  FILLER              PIC X(7) VALUE '06600 E'.                
001200* 5                                                                       
001300         05  FILLER              PIC X(7) VALUE '00300 A'.                
001400         05  FILLER              PIC X(7) VALUE '01110 B'.                
001500         05  FILLER              PIC X(7) VALUE '01120 B'.                
001700         05  FILLER              PIC X(7) VALUE '01320 B'.                
002301         05  FILLER              PIC X(7) VALUE '01400 C'.                
002302* 10                                                                      
002310         05  FILLER              PIC X(7) VALUE '01180 B'.                
002320         05  FILLER              PIC X(7) VALUE '01420 C'.                
003100         05  FILLER              PIC X(7) VALUE '01444 B'.                
003300         05  FILLER              PIC X(7) VALUE '01445 B'.                
003400         05  FILLER              PIC X(7) VALUE '01544 B'.                
003500* 15                                                                      
004300         05  FILLER              PIC X(7) VALUE '01600 C'.                
004310         05  FILLER              PIC X(7) VALUE '01820 B'.                
004400         05  FILLER              PIC X(7) VALUE '01830 B'.                
004401         05  FILLER              PIC X(7) VALUE '02075 D'.                
004403         05  FILLER              PIC X(7) VALUE '02079 D'.                
004404* 20                                                                      
004405         05  FILLER              PIC X(7) VALUE '02085 G'.                
004406         05  FILLER              PIC X(7) VALUE '02088 G'.                
004407         05  FILLER              PIC X(7) VALUE '02185 G'.                
004408         05  FILLER              PIC X(7) VALUE '02188 G'.                
004500         05  FILLER              PIC X(7) VALUE '03085 J'.                
004510* 25                                                                      
004600         05  FILLER              PIC X(7) VALUE '03430 G'.                
004700         05  FILLER              PIC X(7) VALUE '03480 H'.                
004900         05  FILLER              PIC X(7) VALUE '04086 K'.                
005000         05  FILLER              PIC X(7) VALUE '04088 L'.                
005100         05  FILLER              PIC X(7) VALUE '06601 F'.                
005110* 30                                                                      
005200         05  FILLER              PIC X(7) VALUE '07091 J'.                
005300         05  FILLER              PIC X(7) VALUE '07191 J'.                
005310         05  FILLER              PIC X(7) VALUE '07082 F'.                
005410         05  FILLER              PIC X(7) VALUE '07085 H'.                
005420         05  FILLER              PIC X(7) VALUE '07185 H'.                
005421* 35                                                                      
005430         05  FILLER              PIC X(7) VALUE '07488 H'.                
005440         05  FILLER              PIC X(7) VALUE '07491 H'.                
005450         05  FILLER              PIC X(7) VALUE '07588 H'.                
005500         05  FILLER              PIC X(7) VALUE '07591 H'.                
005600         05  FILLER              PIC X(7) VALUE '07688 F'.                
005610* 40                                                                      
005700         05  FILLER              PIC X(7) VALUE '07788 F'.                
005800         05  FILLER              PIC X(7) VALUE '07886 E'.                
005900         05  FILLER              PIC X(7) VALUE '07986 E'.                
006100         05  FILLER              PIC X(7) VALUE '08092 K'.                
006200         05  FILLER              PIC X(7) VALUE '08192 K'.                
006210* 45                                                                      
006300         05  FILLER              PIC X(7) VALUE '09094 J'.                
006400         05  FILLER              PIC X(7) VALUE '09194 J'.                
006410         05  FILLER              PIC X(7) VALUE '11206 B'.                
006420         05  FILLER              PIC X(7) VALUE '14441 B'.                
006500         05  FILLER              PIC X(7) VALUE '99999  '.                
006600* 50                                                                      
006700     03  AGEKODETABELL REDEFINES TABELL1                                  
006800                                OCCURS 50                                 
006900                                ASCENDING TAB1-IDKATNR                    
007000                     INDEXED BY IX.                                       
007100         05  TAB1-IDKATNR        PIC 9(5).                                
007200         05  FILLER              PIC X.                                   
007300         05  TAB1-KDAGE          PIC X.                                   
007400*** END COPY WAGECODE                                                     
