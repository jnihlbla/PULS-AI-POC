000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ20TEFA.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   2002-09-12.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SUBROUTINE TO CHECK THE VALIDITY OF A TELEPHONE OR               
001000*        FAX NUMBER                                                       
001100*                                                                         
001200                                                                          
001300     EJECT                                                                
001400 DATA DIVISION.                                                           
001500     SKIP3                                                                
001600 WORKING-STORAGE SECTION.                                                 
001700                                                                          
001800 77  IDPGM                       PIC X(8)    VALUE 'WZ20TEFA'.            
001900                                                                          
002000 77  OUT-IDTFN-IDTFX             PIC X(20).                               
002100                                                                          
002200 77  INX                         PIC S9(4)   BINARY.                      
002300 77  IN-LENGTH                   PIC S9(4)   BINARY.                      
002400 77  OUTX                        PIC S9(4)   BINARY.                      
002410 77  LREST                       PIC S9(4)   BINARY.                      
002500                                                                          
002600 77  CTX                         PIC S9(4)   BINARY.                      
002610 77  CTX-MAX                     PIC S9(4)   BINARY  VALUE +19.           
002620 77  LCC                         PIC S9(4)   BINARY.                      
002630 77  CC                          PIC X(3).                                
002700                                                                          
003200 01  COUNTRY-NUMBER-TAB.                                                  
003210     03  CNUMB-CA-AND-US         PIC X(3)   VALUE      '1  '.             
003220     03  L-CA-AND-US             PIC S9(4)  BINARY  VALUE +1.             
003230     03  CNUMB-ZA                PIC X(3)   VALUE      '27 '.             
003240     03  L-ZA                    PIC S9(4)  BINARY  VALUE +2.             
003300     03  CNUMB-EU-MISC-3         PIC X(3)   VALUE      '3  '.             
003400     03  L-EU-MISC-3             PIC S9(4)  BINARY  VALUE +1.             
005700     03  CNUMB-EU-MISC-4         PIC X(3)   VALUE      '4  '.             
005800     03  L-EU-MISC-4             PIC S9(4)  BINARY  VALUE +1.             
005900     03  CNUMB-PE                PIC X(3)   VALUE      '51 '.             
006000     03  L-PE                    PIC S9(4)  BINARY  VALUE +2.             
006100     03  CNUMB-MX                PIC X(3)   VALUE      '52 '.             
006200     03  L-MX                    PIC S9(4)  BINARY  VALUE +2.             
006300     03  CNUMB-BR                PIC X(3)   VALUE      '55 '.             
006400     03  L-BR                    PIC S9(4)  BINARY  VALUE +2.             
006500     03  CNUMB-MY                PIC X(3)   VALUE      '60 '.             
006600     03  L-MY                    PIC S9(4)  BINARY  VALUE +2.             
006700     03  CNUMB-AU                PIC X(3)   VALUE      '61 '.             
006800     03  L-AU                    PIC S9(4)  BINARY  VALUE +2.             
006900     03  CNUMB-NZ                PIC X(3)   VALUE      '64 '.             
007000     03  L-NZ                    PIC S9(4)  BINARY  VALUE +2.             
007010     03  CNUMB-TH                PIC X(3)   VALUE      '66 '.             
007020     03  L-TH                    PIC S9(4)  BINARY  VALUE +2.             
007030     03  CNUMB-RU                PIC X(3)   VALUE      '7  '.             
007040     03  L-RU                    PIC S9(4)  BINARY  VALUE +1.             
007300     03  CNUMB-JP                PIC X(3)   VALUE      '81 '.             
007400     03  L-JP                    PIC S9(4)  BINARY  VALUE +2.             
007410     03  CNUMB-TW                PIC X(3)   VALUE      '886'.             
007420     03  L-TW                    PIC S9(4)  BINARY  VALUE +3.             
007430     03  CNUMB-MIDDLE-EAST       PIC X(3)   VALUE      '9  '.             
007440     03  L-MIDDLE-EAST           PIC S9(4)  BINARY  VALUE +1.             
009600                                                                          
009610 01  FILLER REDEFINES COUNTRY-NUMBER-TAB.                                 
009611     03  FILLER       OCCURS 15.                                          
009620       05  CNUMB                   PIC X(3).                              
009630       05  L-CNUMB                 PIC S9(4)  BINARY.                     
009640                                                                          
009700     EJECT                                                                
009800 LINKAGE SECTION.                                                         
009900                                                                          
010000*    -COPY WZ20TEFA                                                       
010100     EJECT                                                                
010200 PROCEDURE DIVISION USING TEFA-WZ20TEFA.                                  
010300 MAIN SECTION.                                                            
010400     SKIP2                                                                
010500     PERFORM A-INIT-GENERAL-CHECK                                         
010600                                                                          
010700     IF TEFA-KDRC = 0                                                     
010800       IF TEFA-IDTFN-IDTFX(1:1) = '+'                                     
010900         PERFORM B-CHECK-COUNTRY-AND-NUMBER                               
011000       ELSE                                                               
011100         IF IN-LENGTH > 6                                                 
011200           PERFORM C-CHECK-AREA-AND-NUMBER                                
011201         ELSE                                                             
011210           PERFORM D-CHECK-SIMPLE-NUMBER                                  
011300         END-IF                                                           
011310       END-IF                                                             
011311     END-IF                                                               
011312                                                                          
011313     IF TEFA-KDRC = 0                                                     
011314       IF OUT-IDTFN-IDTFX NOT = TEFA-IDTFN-IDTFX                          
011320         MOVE OUT-IDTFN-IDTFX TO TEFA-IDTFN-IDTFX                         
011321         MOVE 4 TO TEFA-KDRC                                              
011322       END-IF                                                             
011330     END-IF                                                               
011700                                                                          
011800     MOVE ZERO TO RETURN-CODE                                             
011900     GOBACK                                                               
012000     .                                                                    
012100     EJECT                                                                
012200 A-INIT-GENERAL-CHECK     SECTION.                                        
012300                                                                          
012400     MOVE ZERO TO TEFA-KDRC                                               
012500                                                                          
012600*    -- COMPUTE THE ACTUAL LENGTH                                         
012700     MOVE ZERO TO IN-LENGTH                                               
012800     INSPECT FUNCTION REVERSE (TEFA-IDTFN-IDTFX)                          
012900     TALLYING IN-LENGTH FOR LEADING SPACE.                                
013000     COMPUTE IN-LENGTH = 20 - IN-LENGTH                                   
013100                                                                          
013200*    -- COMPUTE STARTING POINT FOR NUMBER                                 
013300     MOVE 1 TO INX                                                        
013400     IF TEFA-IDTFN-IDTFX (1:1) = '+'                                      
013500       MOVE 2 TO INX                                                      
013700     END-IF                                                               
013800     IF IN-LENGTH < 6                                                     
013900       MOVE 8 TO TEFA-KDRC                                                
014000     ELSE                                                                 
014100       IF TEFA-IDTFN-IDTFX (INX:IN-LENGTH - INX + 1) NOT NUMERIC          
014200         MOVE 8 TO TEFA-KDRC                                              
014300       END-IF                                                             
014400     END-IF                                                               
014500                                                                          
014600     MOVE '+' TO OUT-IDTFN-IDTFX                                          
014700     MOVE 2 TO OUTX                                                       
014800     .                                                                    
014810                                                                          
014900     EJECT                                                                
015000 B-CHECK-COUNTRY-AND-NUMBER   SECTION.                                    
015100                                                                          
015110     MOVE 1 TO CTX                                                        
015120     MOVE 8 TO TEFA-KDRC                                                  
015200     PERFORM UNTIL CTX = CTX-MAX OR TEFA-KDRC = ZERO                      
015210       MOVE L-CNUMB(CTX) TO  LCC                                          
015220       MOVE CNUMB(CTX)   TO  CC                                           
015300       IF TEFA-IDTFN-IDTFX(INX:LCC) = CC(1:LCC)                           
015400         MOVE ZERO TO TEFA-KDRC                                           
015500       END-IF                                                             
015600       ADD 1 TO CTX                                                       
015900     END-PERFORM                                                          
015910                                                                          
015911*    -- MOVE THE COUNTRY CODE                                             
015912     MOVE TEFA-IDTFN-IDTFX(INX:LCC) TO OUT-IDTFN-IDTFX(OUTX:LCC)          
015913     ADD LCC TO INX                                                       
015914     ADD LCC TO OUTX                                                      
015915                                                                          
015916     COMPUTE LREST = IN-LENGTH - INX + 1                                  
015920     IF LREST < 7                                                         
015930*      -- AREA CODE + NUMBER < 7 DIGITS - TOO SHORT                       
015940       MOVE 8 TO TEFA-KDRC                                                
015950     ELSE                                                                 
015960       MOVE TEFA-IDTFN-IDTFX(INX:LREST)                                   
015961         TO OUT-IDTFN-IDTFX(OUTX:LREST)                                   
015970     END-IF                                                               
016000     .                                                                    
016001                                                                          
016010     EJECT                                                                
016100 C-CHECK-AREA-AND-NUMBER  SECTION.                                        
016200                                                                          
016201*    -- NO COUNTRY CODE -- ASSUME SWEDEN                                  
016202                                                                          
016210*    -- ADD SWEDEN'S COUNTRY CODE                                         
016220     MOVE '46' TO  OUT-IDTFN-IDTFX(OUTX:2)                                
016221     ADD 2 TO OUTX                                                        
016222                                                                          
016224     IF TEFA-IDTFN-IDTFX(1:1) NOT = '0'                                   
016225*      -- INVALID SWEDISH AREA CODE                                       
016230       MOVE 8 TO TEFA-KDRC                                                
016231     ELSE                                                                 
016232*      -- IGNORE LEADING ZERO IN AREA CODE                                
016233       ADD 1 TO INX                                                       
016234       COMPUTE LREST = IN-LENGTH - INX + 1                                
016235       IF LREST < 8                                                       
016236*        -- AREA CODE + NUMBER < 8 DIGITS - TOO SHORT                     
016237         MOVE 8 TO TEFA-KDRC                                              
016238       ELSE                                                               
016239         MOVE TEFA-IDTFN-IDTFX(INX:LREST)                                 
016240         TO OUT-IDTFN-IDTFX(OUTX:LREST)                                   
016241       END-IF                                                             
016242     END-IF                                                               
016300     .                                                                    
016310                                                                          
016400     EJECT                                                                
016500 D-CHECK-SIMPLE-NUMBER  SECTION.                                          
016600                                                                          
016610*    -- LENGTH <= 6 DIGITS -- ASSUME GOTHENBURG NUMBER                    
016620                                                                          
016630*    -- ADD SWEDEN'S COUNTRY CODE AND GOT AREA CODE                       
016640     MOVE '4631' TO  OUT-IDTFN-IDTFX(OUTX:4)                              
016650     ADD 4 TO OUTX                                                        
016660                                                                          
016694     COMPUTE LREST = IN-LENGTH                                            
016695     IF LREST < 6 OR TEFA-IDTFN-IDTFX(1:1) = '0'                          
016696*      -- NUMBER < 6 DIGITS, TOO SHORT                                    
016697*      -- OR LEADING ZERO (ONLY ALLOWED IN AREA CODE)                     
016698       MOVE 8 TO TEFA-KDRC                                                
016699     ELSE                                                                 
016700       MOVE TEFA-IDTFN-IDTFX(1:LREST)                                     
016701       TO OUT-IDTFN-IDTFX(OUTX:LREST)                                     
016702     END-IF                                                               
016800     .                                                                    
