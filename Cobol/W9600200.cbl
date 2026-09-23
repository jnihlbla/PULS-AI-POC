000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9600200.                                                
000400 AUTHOR.         KARIN OLSSON.                                            
000500 DATE-WRITTEN.   92/12/18.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ANROPAS VID BROWSE AV EN KOMPILERINGSLISTA.                      
001100*        KOMPILERINGSLISTAN KAN VARA AV TYP COBOL, EPLUS                  
001200*        ASSEMBLER ELLER FRÅN EN PSB-GENERERING.                          
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600     SKIP2                                                                
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(8)    VALUE 'W9600200'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000     SKIP2                                                                
003100 77  LISTMEM-EOF-SW              PIC X       VALUE 'N'.                   
003200     88  END-OF-LISTMEM                      VALUE 'J'.                   
003300 77  COBOL-TYP                   PIC X       VALUE 'C'.                   
003400 77  EPLUS-TYP                   PIC X       VALUE 'E'.                   
003500 77  ASM-TYP                     PIC X       VALUE 'A'.                   
003600 77  PSB-TYP                     PIC X       VALUE 'P'.                   
003610 77  FORMAT-TYP                  PIC X       VALUE 'F'.                   
003620 77  COBOL-PANEL                 PIC X(8)    VALUE 'LISTBRCO'.            
003630 77  EPLUS-PANEL                 PIC X(8)    VALUE 'LISTBREZ'.            
003640 77  ASM-PANEL                   PIC X(8)    VALUE 'LISTBRAS'.            
003650 77  PSB-PANEL                   PIC X(8)    VALUE 'LISTBRPS'.            
003660 77  FORMAT-PANEL                PIC X(8)    VALUE 'LISTBRFO'.            
003670 77  DEFAULT-PANEL               PIC X(8)    VALUE 'LISTBR00'.            
003700     SKIP2                                                                
003800 77  ISP-BRIF                    PIC X(8)    VALUE 'BRIF    '.            
003900 77  ISP-VGET                    PIC X(8)    VALUE 'VGET    '.            
004000 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
004100 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
004200 77  ISP-VDELETE                 PIC X(8)    VALUE 'VDELETE '.            
004300 77  ISP-VRESET                  PIC X(8)    VALUE 'VRESET  '.            
004400 77  ISP-DISPLAY                 PIC X(8)    VALUE 'DISPLAY '.            
004500 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
004600 77  ISP-PROFILE                 PIC X(8)    VALUE 'PROFILE '.            
004700 77  ISP-CONTROL                 PIC X(8)    VALUE 'CONTROL '.            
004800 77  ISP-CANCEL                  PIC X(8)    VALUE 'CANCEL  '.            
004900 77  ISP-RETURN                  PIC X(8)    VALUE 'RETURN  '.            
005000 77  ISP-ERRORS                  PIC X(8)    VALUE 'ERRORS  '.            
005100 77  SETMSG                      PIC X(8)    VALUE 'SETMSG  '.            
005200 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
005300 77  PACK                        PIC X(8)    VALUE 'PACK    '.            
005400 77  VDEFINE-OPT                 PIC X(16)                                
005500                              VALUE '(COPY NOBSCAN)'.                     
005600 77  LMINIT                      PIC X(8)    VALUE 'LMINIT  '.            
005700 77  LMOPEN                      PIC X(8)    VALUE 'LMOPEN  '.            
005800 77  LMGET                       PIC X(8)    VALUE 'LMGET   '.            
005900 77  LMFREE                      PIC X(8)    VALUE 'LMFREE  '.            
006000 77  LMCLOSE                     PIC X(8)    VALUE 'LMCLOSE '.            
006100 77  ISP-INPUT                   PIC X(8)    VALUE 'INPUT   '.            
006200 77  ISP-INVAR                   PIC X(8)    VALUE 'INVAR   '.            
006300 77  ISP-REPLACE                 PIC X(8)    VALUE 'REPLACE '.            
006400 77  ISP-SHR                     PIC X(8)    VALUE 'SHR     '.            
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007000     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
007100     03  W9600210                PIC X(8)    VALUE 'W9600210'.            
007200     03  W9600220                PIC X(8)    VALUE 'W9600220'.            
007300     03  W9600230                PIC X(8)    VALUE 'W9600230'.            
007400     03  WDELETE                 PIC X(8)    VALUE 'WDELETE'.             
007500     EJECT                                                                
007600* --- PARAMETRAR TILL ABEND                                               
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     EJECT                                                                
008100* --- PARAMETRAR TILL WDECEDIT                                            
008200                                                                          
008300*01  -COPY WDECAREA                                                       
008400     EJECT                                                                
008500 01  WPANEL                      PIC X(8).                                
008600 01  WLISTTYP                    PIC X.                                   
008700 01  WDUMMY                      PIC X       VALUE SPACE.                 
008800 01  WDSNAME                     PIC X(46).                               
008900 01  RECFM                       PIC X(2)    VALUE SPACE.                 
009000 01  LRECL                       PIC S9(6)   COMP.                        
009100 01  MAXLRECL                    PIC S9(9)   COMP SYNC.                   
009200 01  READPTR                     PIC S9(8)   COMP SYNC.                   
009300 01  CMDPTR                      PIC S9(8)   COMP SYNC.                   
009400 01  DATAPTR                     PIC S9(8)   COMP SYNC.                   
009500 01  TAB-IX                      PIC S9(5)   COMP VALUE ZERO.             
009600     SKIP2                                                                
009700 01  VARLRECL                    PIC X(8).                                
009800 01  N-VARLRECL                  PIC X(8)    VALUE 'VARLRECL'.            
009900 01  L-VARLRECL                  PIC S9(9)   COMP VALUE +8.               
010000     SKIP2                                                                
010100 01  ZERRMSG                     PIC X(8).                                
010200 01  N-ZERRMSG                   PIC X(8)    VALUE 'ZERRMSG'.             
010300 01  L-ZERRMSG                   PIC S9(9)   COMP VALUE +8.               
010400     SKIP2                                                                
010500 01  DDVAR                       PIC X(8).                                
010600 01  N-DDVAR                     PIC X(8)    VALUE 'DDVAR'.               
010700 01  L-DDVAR                     PIC S9(9)   COMP VALUE +8.               
010800     SKIP2                                                                
010900 01  LISTLIB                     PIC X(44).                               
011000 01  N-LISTLIB                   PIC X(8)    VALUE 'LISTLIB'.             
011100 01  L-LISTLIB                   PIC S9(9)   COMP VALUE +44.              
011200     SKIP2                                                                
011300 01  LISTMEM                     PIC X(8).                                
011400 01  N-LISTMEM                   PIC X(8)    VALUE 'LISTMEM'.             
011500 01  L-LISTMEM                   PIC S9(9)   COMP VALUE +8.               
011600     SKIP2                                                                
011700 01  LIBRECFM                    PIC X(4).                                
011800 01  N-LIBRECFM                  PIC X(8)    VALUE 'LIBRECFM'.            
011900 01  L-LIBRECFM                  PIC S9(9)   COMP VALUE +4.               
012000     SKIP2                                                                
012100 01  LIBLRECL                    PIC X(8).                                
012200 01  N-LIBLRECL                  PIC X(8)    VALUE 'LIBLRECL'.            
012300 01  L-LIBLRECL                  PIC S9(9)   COMP VALUE +8.               
012400     SKIP2                                                                
012500 01  RADLRECL                    PIC 9(3)    COMP-3.                      
012600 01  N-RADLRECL                  PIC X(8)    VALUE 'RADLRECL'.            
012700 01  L-RADLRECL                  PIC S9(9)   COMP VALUE +2.               
012800     SKIP2                                                                
012900 01  MEMAREA                     PIC X(150).                              
013000 01  N-MEMAREA                   PIC X(8)    VALUE 'MEMAREA'.             
013100 01  L-MEMAREA                   PIC S9(9)   COMP VALUE +150.             
013200     SKIP2                                                                
013300 01  TOPRADNR                    PIC 9(5)    COMP-3.                      
013400 01  N-TOPRADNR                  PIC X(8)    VALUE 'TOPRADNR'.            
013500 01  L-TOPRADNR                  PIC S9(9)   COMP VALUE +3.               
013600     SKIP2                                                                
013700 01  MAXRADNR                    PIC 9(5)    COMP-3.                      
013800 01  N-MAXRADNR                  PIC X(8)    VALUE 'MAXRADNR'.            
013900 01  L-MAXRADNR                  PIC S9(9)   COMP VALUE +3.               
014000     SKIP2                                                                
014100 01  SECT                        PIC X(1).                                
014200 01  N-SECT                      PIC X(8)    VALUE 'SECT'.                
014300 01  L-SECT                      PIC S9(9)   COMP VALUE +1.               
014400     SKIP2                                                                
014500 01  NYSECT                      PIC X(1).                                
014600 01  N-NYSECT                    PIC X(8)    VALUE 'NYSECT'.              
014700 01  L-NYSECT                    PIC S9(9)   COMP VALUE +1.               
014800     SKIP2                                                                
014900 01  SELSECT                     PIC X(1).                                
015000 01  N-SELSECT                   PIC X(8)    VALUE 'SELSECT'.             
015100 01  L-SELSECT                   PIC S9(9)   COMP VALUE +1.               
015200     SKIP2                                                                
015300 01  MAXSECT                     PIC X(1).                                
015400 01  N-MAXSECT                   PIC X(8)    VALUE 'MAXSECT'.             
015500 01  L-MAXSECT                   PIC S9(9)   COMP VALUE +1.               
015600     SKIP2                                                                
015700 01  START2                      PIC 9(5)    COMP-3.                      
015800 01  N-START2                    PIC X(8)    VALUE 'START2'.              
015900 01  L-START                     PIC S9(9)   COMP VALUE +3.               
016000     SKIP2                                                                
016100 01  START3                      PIC 9(5)    COMP-3.                      
016200 01  N-START3                    PIC X(8)    VALUE 'START3'.              
016300     SKIP2                                                                
016400 01  START4                      PIC 9(5)    COMP-3.                      
016500 01  N-START4                    PIC X(8)    VALUE 'START4'.              
016600     SKIP2                                                                
016700 01  START5                      PIC 9(5)    COMP-3.                      
016800 01  N-START5                    PIC X(8)    VALUE 'START5'.              
016900     SKIP2                                                                
017000 01  START6                      PIC 9(5)    COMP-3.                      
017100 01  N-START6                    PIC X(8)    VALUE 'START6'.              
017200     SKIP2                                                                
017300 01  START7                      PIC 9(5)    COMP-3.                      
017400 01  N-START7                    PIC X(8)    VALUE 'START7'.              
017500     SKIP2                                                                
017600 01  START8                      PIC 9(5)    COMP-3.                      
017700 01  N-START8                    PIC X(8)    VALUE 'START8'.              
017800     SKIP2                                                                
017900 01  MAX1                        PIC 9(5)    COMP-3.                      
018000 01  N-MAX1                      PIC X(8)    VALUE 'MAX1'.                
018100 01  L-MAX                       PIC S9(9)   COMP VALUE +3.               
018200     SKIP2                                                                
018300 01  MAX2                        PIC 9(5)    COMP-3.                      
018400 01  N-MAX2                      PIC X(8)    VALUE 'MAX2'.                
018500     SKIP2                                                                
018600 01  MAX3                        PIC 9(5)    COMP-3.                      
018700 01  N-MAX3                      PIC X(8)    VALUE 'MAX3'.                
018800     SKIP2                                                                
018900 01  MAX4                        PIC 9(5)    COMP-3.                      
019000 01  N-MAX4                      PIC X(8)    VALUE 'MAX4'.                
019100     SKIP2                                                                
019200 01  MAX5                        PIC 9(5)    COMP-3.                      
019300 01  N-MAX5                      PIC X(8)    VALUE 'MAX5'.                
019400     SKIP2                                                                
019500 01  MAX6                        PIC 9(5)    COMP-3.                      
019600 01  N-MAX6                      PIC X(8)    VALUE 'MAX6'.                
019700     SKIP2                                                                
019800 01  MAX7                        PIC 9(5)    COMP-3.                      
019900 01  N-MAX7                      PIC X(8)    VALUE 'MAX7'.                
020000     SKIP2                                                                
020100 01  MAX8                        PIC 9(5)    COMP-3.                      
020200 01  N-MAX8                      PIC X(8)    VALUE 'MAX8'.                
020300     SKIP2                                                                
020400 01  MAXTOT                      PIC 9(5)    COMP-3.                      
020500     SKIP2                                                                
020600 01  RADTAB.                                                              
020700     03  DATARAD OCCURS 50000 PIC X(150).                                 
020800     EJECT                                                                
020900 PROCEDURE DIVISION.                                                      
021000     SKIP2                                                                
021100     MOVE ZERO TO RETURN-CODE                                             
021200                                                                          
021300     PERFORM A-INIT                                                       
021400     PERFORM S01-LAES-LISTMEM                                             
021500     IF NOT END-OF-LISTMEM                                                
021600       PERFORM UNTIL END-OF-LISTMEM                                       
021700         PERFORM B-STOPPA-IN-I-TABELL                                     
021800         PERFORM S01-LAES-LISTMEM                                         
021900       END-PERFORM                                                        
022000       PERFORM C-LAGRA-VARIABLER                                          
022100       CALL ISPLINK USING ISP-DISPLAY WPANEL ZERRMSG                      
022200       PERFORM UNTIL RETURN-CODE > 0                                      
022300         MOVE SPACE TO ZERRMSG                                            
022400         PERFORM D-NYA-VARIABLER                                          
022500         PERFORM E-BROWSA-LISTA                                           
022600         CALL ISPLINK USING ISP-DISPLAY WPANEL ZERRMSG                    
022700       END-PERFORM                                                        
022800     END-IF                                                               
022900                                                                          
023000     PERFORM Z-FINIT                                                      
023100     GOBACK                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500     SKIP2                                                                
023600     MOVE DEFAULT-PANEL TO WPANEL                                         
023610     MOVE SPACE TO WLISTTYP                                               
023700                                                                          
023800     CALL ISPLINK USING ISP-VDEFINE N-SELSECT SELSECT CHAR                
023900                         L-SELSECT VDEFINE-OPT                            
024000     IF RETURN-CODE > 8                                                   
024100       DISPLAY 'KAN INTE VDEFFA SELSECT'                                  
024200       PERFORM S99-ABEND                                                  
024300     END-IF                                                               
024400                                                                          
024500     CALL ISPLINK USING ISP-VDEFINE N-MAXSECT MAXSECT CHAR                
024600                         L-MAXSECT VDEFINE-OPT                            
024700     IF RETURN-CODE > 8                                                   
024800       DISPLAY 'KAN INTE VDEFFA MAXSECT'                                  
024900       PERFORM S99-ABEND                                                  
025000     END-IF                                                               
025100                                                                          
025200     CALL ISPLINK USING ISP-VDEFINE N-ZERRMSG ZERRMSG CHAR                
025300                         L-ZERRMSG VDEFINE-OPT                            
025400     IF RETURN-CODE > 8                                                   
025500       DISPLAY 'KAN INTE VDEFFA ZERRMSG'                                  
025600       PERFORM S99-ABEND                                                  
025700     END-IF                                                               
025800                                                                          
025900     CALL ISPLINK USING ISP-VDEFINE N-DDVAR DDVAR CHAR                    
026000                         L-DDVAR VDEFINE-OPT                              
026100     IF RETURN-CODE > 8                                                   
026200       DISPLAY 'KAN INTE VDEFFA DDVAR'                                    
026300       PERFORM S99-ABEND                                                  
026400     END-IF                                                               
026500                                                                          
026600     CALL ISPLINK USING ISP-VDEFINE N-LISTLIB LISTLIB CHAR                
026700                         L-LISTLIB VDEFINE-OPT                            
026800     IF RETURN-CODE > 0                                                   
026900       DISPLAY 'SAKNAR NAMN PÅ LISTBIBLIOTEK'                             
027000       PERFORM S99-ABEND                                                  
027100     END-IF                                                               
027200                                                                          
027300     CALL ISPLINK USING ISP-VDEFINE N-LISTMEM LISTMEM CHAR                
027400                         L-LISTMEM VDEFINE-OPT                            
027500     IF RETURN-CODE > 0                                                   
027600       DISPLAY 'SAKNAR NAMN PÅ LISTMEDLEM'                                
027700       PERFORM S99-ABEND                                                  
027800     END-IF                                                               
027900                                                                          
028000     CALL ISPLINK USING ISP-VDEFINE N-LIBRECFM LIBRECFM CHAR              
028100                         L-LIBRECFM VDEFINE-OPT                           
028200     IF RETURN-CODE > 8                                                   
028300       DISPLAY 'KAN INTE VDEFFA LIBRECFM'                                 
028400       PERFORM S99-ABEND                                                  
028500     END-IF                                                               
028600                                                                          
028700     CALL ISPLINK USING ISP-VDEFINE N-LIBLRECL LIBLRECL CHAR              
028800                         L-LIBLRECL VDEFINE-OPT                           
028900     IF RETURN-CODE > 8                                                   
029000       DISPLAY 'KAN INTE VDEFFA LIBLRECL'                                 
029100       PERFORM S99-ABEND                                                  
029200     END-IF                                                               
029300                                                                          
029400     CALL ISPLINK USING ISP-VDEFINE N-RADLRECL RADLRECL PACK              
029500                         L-RADLRECL VDEFINE-OPT                           
029600     IF RETURN-CODE > 8                                                   
029700       DISPLAY 'KAN INTE VDEFFA RADLRECL'                                 
029800       PERFORM S99-ABEND                                                  
029900     END-IF                                                               
030000                                                                          
030100     CALL ISPLINK USING ISP-VDEFINE N-MEMAREA MEMAREA CHAR                
030200                         L-MEMAREA VDEFINE-OPT                            
030300     IF RETURN-CODE > 8                                                   
030400       DISPLAY 'KAN INTE VDEFFA MEMAREA'                                  
030500       PERFORM S99-ABEND                                                  
030600     END-IF                                                               
030700                                                                          
030800     CALL ISPLINK USING ISP-VDEFINE N-START2 START2 PACK                  
030900                         L-START VDEFINE-OPT                              
031000     IF RETURN-CODE > 8                                                   
031100       DISPLAY 'KAN INTE VDEFFA START2'                                   
031200       PERFORM S99-ABEND                                                  
031300     END-IF                                                               
031400                                                                          
031500     CALL ISPLINK USING ISP-VDEFINE N-START3 START3 PACK                  
031600                         L-START VDEFINE-OPT                              
031700     IF RETURN-CODE > 8                                                   
031800       DISPLAY 'KAN INTE VDEFFA START3'                                   
031900       PERFORM S99-ABEND                                                  
032000     END-IF                                                               
032100                                                                          
032200     CALL ISPLINK USING ISP-VDEFINE N-START4 START4 PACK                  
032300                         L-START VDEFINE-OPT                              
032400     IF RETURN-CODE > 8                                                   
032500       DISPLAY 'KAN INTE VDEFFA START4'                                   
032600       PERFORM S99-ABEND                                                  
032700     END-IF                                                               
032800                                                                          
032900     CALL ISPLINK USING ISP-VDEFINE N-START5 START5 PACK                  
033000                         L-START VDEFINE-OPT                              
033100     IF RETURN-CODE > 8                                                   
033200       DISPLAY 'KAN INTE VDEFFA START5'                                   
033300       PERFORM S99-ABEND                                                  
033400     END-IF                                                               
033500                                                                          
033600     CALL ISPLINK USING ISP-VDEFINE N-START6 START6 PACK                  
033700                         L-START VDEFINE-OPT                              
033800     IF RETURN-CODE > 8                                                   
033900       DISPLAY 'KAN INTE VDEFFA START6'                                   
034000       PERFORM S99-ABEND                                                  
034100     END-IF                                                               
034200                                                                          
034300     CALL ISPLINK USING ISP-VDEFINE N-START7 START7 PACK                  
034400                         L-START VDEFINE-OPT                              
034500     IF RETURN-CODE > 8                                                   
034600       DISPLAY 'KAN INTE VDEFFA START7'                                   
034700       PERFORM S99-ABEND                                                  
034800     END-IF                                                               
034900                                                                          
035000     CALL ISPLINK USING ISP-VDEFINE N-START8 START8 PACK                  
035100                         L-START VDEFINE-OPT                              
035200     IF RETURN-CODE > 8                                                   
035300       DISPLAY 'KAN INTE VDEFFA START8'                                   
035400       PERFORM S99-ABEND                                                  
035500     END-IF                                                               
035600                                                                          
035700     CALL ISPLINK USING ISP-VDEFINE N-MAX1 MAX1 PACK                      
035800                         L-MAX VDEFINE-OPT                                
035900     IF RETURN-CODE > 8                                                   
036000       DISPLAY 'KAN INTE VDEFFA MAX1'                                     
036100       PERFORM S99-ABEND                                                  
036200     END-IF                                                               
036300                                                                          
036400     CALL ISPLINK USING ISP-VDEFINE N-MAX2 MAX2 PACK                      
036500                         L-MAX VDEFINE-OPT                                
036600     IF RETURN-CODE > 8                                                   
036700       DISPLAY 'KAN INTE VDEFFA MAX2'                                     
036800       PERFORM S99-ABEND                                                  
036900     END-IF                                                               
037000                                                                          
037100     CALL ISPLINK USING ISP-VDEFINE N-MAX3 MAX3 PACK                      
037200                         L-MAX VDEFINE-OPT                                
037300     IF RETURN-CODE > 8                                                   
037400       DISPLAY 'KAN INTE VDEFFA MAX3'                                     
037500       PERFORM S99-ABEND                                                  
037600     END-IF                                                               
037700                                                                          
037800     CALL ISPLINK USING ISP-VDEFINE N-MAX4 MAX4 PACK                      
037900                         L-MAX VDEFINE-OPT                                
038000     IF RETURN-CODE > 8                                                   
038100       DISPLAY 'KAN INTE VDEFFA MAX4'                                     
038200       PERFORM S99-ABEND                                                  
038300     END-IF                                                               
038400                                                                          
038500     CALL ISPLINK USING ISP-VDEFINE N-MAX5 MAX5 PACK                      
038600                         L-MAX VDEFINE-OPT                                
038700     IF RETURN-CODE > 8                                                   
038800       DISPLAY 'KAN INTE VDEFFA MAX5'                                     
038900       PERFORM S99-ABEND                                                  
039000     END-IF                                                               
039100                                                                          
039200     CALL ISPLINK USING ISP-VDEFINE N-MAX6 MAX6 PACK                      
039300                         L-MAX VDEFINE-OPT                                
039400     IF RETURN-CODE > 8                                                   
039500       DISPLAY 'KAN INTE VDEFFA MAX6'                                     
039600       PERFORM S99-ABEND                                                  
039700     END-IF                                                               
039800                                                                          
039900     CALL ISPLINK USING ISP-VDEFINE N-MAX7 MAX7 PACK                      
040000                         L-MAX VDEFINE-OPT                                
040100     IF RETURN-CODE > 8                                                   
040200       DISPLAY 'KAN INTE VDEFFA MAX7'                                     
040300       PERFORM S99-ABEND                                                  
040400     END-IF                                                               
040500                                                                          
040600     CALL ISPLINK USING ISP-VDEFINE N-MAX8 MAX8 PACK                      
040700                         L-MAX VDEFINE-OPT                                
040800     IF RETURN-CODE > 8                                                   
040900       DISPLAY 'KAN INTE VDEFFA MAX8'                                     
041000       PERFORM S99-ABEND                                                  
041100     END-IF                                                               
041200                                                                          
041300     MOVE SPACE TO WDSNAME                                                
041400     STRING '''' LISTLIB '.' LISTMEM ''''                                 
041500         DELIMITED BY SPACE INTO WDSNAME                                  
041600                                                                          
041700     CALL ISPLINK USING LMINIT N-DDVAR WDUMMY WDUMMY WDUMMY               
041800                        WDUMMY WDUMMY WDUMMY WDSNAME WDUMMY               
041900                        WDUMMY WDUMMY ISP-SHR                             
042000     IF RETURN-CODE > 0                                                   
042100       DISPLAY 'LMINIT RETURN CODE: ' RETURN-CODE                         
042200       PERFORM S99-ABEND                                                  
042300     END-IF                                                               
042400                                                                          
042500     CALL ISPLINK USING LMOPEN DDVAR ISP-INPUT N-LIBLRECL                 
042600                        N-LIBRECFM WDUMMY                                 
042700     IF RETURN-CODE > 0                                                   
042810       DISPLAY 'LMOPEN RETURN CODE: ' RETURN-CODE                         
042900       PERFORM S99-ABEND                                                  
043000     END-IF                                                               
043100                                                                          
043200     EVALUATE TRUE                                                        
043300                                                                          
043400       WHEN LIBRECFM = 'F' OR 'FB'                                        
043500         MOVE 'F' TO RECFM                                                
043600                                                                          
043700       WHEN LIBRECFM = 'FA' OR 'FBA'                                      
043800         MOVE 'FA' TO RECFM                                               
043900                                                                          
044000       WHEN LIBRECFM = 'V' OR 'VB'                                        
044100         MOVE 'V' TO RECFM                                                
044200                                                                          
044300       WHEN LIBRECFM = 'VA' OR 'VBA'                                      
044400         MOVE 'VA' TO RECFM                                               
044500                                                                          
044600       WHEN OTHER                                                         
044700         DISPLAY 'RECORD FORMAT ' LIBRECFM ' IS NOT SUPPORTED.'           
044800         PERFORM S99-ABEND                                                
044900     END-EVALUATE                                                         
045000                                                                          
045100     IF LIBLRECL = SPACE                                                  
045200       DISPLAY 'SAKNAR POSTLÄNGD PÅ LISTBIBLIOTEK'                        
045300       PERFORM S99-ABEND                                                  
045400     END-IF                                                               
045500                                                                          
045600     MOVE LIBLRECL TO DEC-IDFRIDATA                                       
045700     MOVE 3        TO DEC-KVHELTAL                                        
045800     MOVE 0        TO DEC-KVDECIMAL                                       
045900                                                                          
046000     CALL WDECEDIT USING DEC-WDECAREA                                     
046100                                                                          
046200     IF DEC-KDSVAR-FEL                                                    
046300       DISPLAY 'FEL FRÅN WDECEDIT'                                        
046400       PERFORM S99-ABEND                                                  
046500     END-IF                                                               
046600                                                                          
046700     MOVE DEC-IDEDITDATA TO LRECL MAXLRECL RADLRECL                       
046800     IF LRECL > 150                                                       
046900       DISPLAY 'MAX POSTLÄNGD 150'                                        
047000       PERFORM S99-ABEND                                                  
047100     END-IF                                                               
047200                                                                          
047300     MOVE 0 TO TAB-IX                                                     
047400     MOVE ZERO TO READPTR CMDPTR DATAPTR                                  
047500                                                                          
047600     MOVE ZERO TO START2 START3 START4 START5 START6 START7 START8        
047700     MOVE ZERO TO MAX1 MAX2 MAX3 MAX4 MAX5 MAX6 MAX7 MAX8                 
047800                                                                          
047900     MOVE SPACE TO RADTAB                                                 
047910     MOVE 0 TO MAXSECT                                                    
048000     .                                                                    
048100     EJECT                                                                
048200 B-STOPPA-IN-I-TABELL  SECTION.                                           
048300     SKIP2                                                                
048400     IF TAB-IX = 50000                                                    
048500     DISPLAY 'FULL TABELL. MAX 50000 RADER. ÖKA OCH KOMPILERA OM.'        
048600       PERFORM S99-ABEND                                                  
048700     END-IF                                                               
048800                                                                          
048900     ADD +1 TO TAB-IX                                                     
049000     MOVE MEMAREA TO DATARAD (TAB-IX)                                     
049100     EVALUATE TRUE                                                        
049200                                                                          
049300       WHEN WLISTTYP = COBOL-TYP                                          
049400                                                                          
049500         EVALUATE TRUE                                                    
049600           WHEN MEMAREA (21:10) = 'V12350-001'                            
049700                AND START2 = ZERO                                         
049800             SUBTRACT 1 FROM TAB-IX GIVING START2 MAX1                    
049900                                                                          
050000           WHEN MEMAREA (2:19) = 'REPORT WRITER'                          
050100                AND START3 = ZERO                                         
050200             SUBTRACT 1 FROM TAB-IX GIVING START3 MAX2                    
050300             IF START2 = ZERO                                             
050400               MOVE MAX2 TO MAX1                                          
050500               MOVE ZERO TO MAX2                                          
050600             ELSE                                                         
050700               SUBTRACT START2 FROM MAX2                                  
050800             END-IF                                                       
050900                                                                          
051000           WHEN MEMAREA (2:19) = 'DB2 SQL PRECOMPILER'                    
051100                AND START4 = ZERO                                         
051200             SUBTRACT 1 FROM TAB-IX GIVING START4 MAX3                    
051300             IF START3 = ZERO                                             
051400               IF START2 = ZERO                                           
051500                 MOVE MAX3 TO MAX1                                        
051600               ELSE                                                       
051700                 MOVE MAX3 TO MAX2                                        
051800                 SUBTRACT START2 FROM MAX2                                
051900               END-IF                                                     
052000               MOVE ZERO TO MAX3                                          
052100             ELSE                                                         
052200               SUBTRACT START3 FROM MAX3                                  
052300             END-IF                                                       
052400                                                                          
052500           WHEN MEMAREA (14:15) = 'IBM VS COBOL II'                       
052600                AND START5 = ZERO                                         
052700             SUBTRACT 1 FROM TAB-IX GIVING START5 MAX4                    
052800             IF START4 = ZERO                                             
052900               IF START3 = ZERO                                           
053000                 IF START2 = ZERO                                         
053100                   MOVE MAX4 TO MAX1                                      
053200                 ELSE                                                     
053300                   MOVE MAX4 TO MAX2                                      
053400                   SUBTRACT START2 FROM MAX2                              
053500                 END-IF                                                   
053600               ELSE                                                       
053700                 MOVE MAX4 TO MAX3                                        
053800                 SUBTRACT START3 FROM MAX3                                
053900               END-IF                                                     
054000               MOVE ZERO TO MAX4                                          
054100             ELSE                                                         
054200               SUBTRACT START4 FROM MAX4                                  
054300             END-IF                                                       
054400                                                                          
054500           WHEN MEMAREA (11:24) = 'Cross-reference of verbs'              
054600                AND START6 = ZERO                                         
054700             SUBTRACT 2 FROM TAB-IX GIVING START6 MAX5                    
054800             IF START5 = ZERO                                             
054900               IF START4 = ZERO                                           
055000                 IF START3 = ZERO                                         
055100                   IF START2 = ZERO                                       
055200                     MOVE MAX5 TO MAX1                                    
055300                   ELSE                                                   
055400                     MOVE MAX5 TO MAX2                                    
055500                     SUBTRACT START2 FROM MAX2                            
055600                   END-IF                                                 
055700                 ELSE                                                     
055800                   MOVE MAX5 TO MAX3                                      
055900                   SUBTRACT START3 FROM MAX3                              
056000                 END-IF                                                   
056100               ELSE                                                       
056200                 MOVE MAX5 TO MAX4                                        
056300                 SUBTRACT START4 FROM MAX4                                
056400               END-IF                                                     
056500               MOVE ZERO TO MAX5                                          
056600             ELSE                                                         
056700               SUBTRACT START5 FROM MAX5                                  
056800             END-IF                                                       
056900                                                                          
057000           WHEN MEMAREA (31:14) = 'LINKAGE EDITOR'                        
057100                AND START7 = ZERO                                         
057200             SUBTRACT 2 FROM TAB-IX GIVING START7 MAX6                    
057300             IF START6 = ZERO                                             
057400               IF START5 = ZERO                                           
057500                 IF START4 = ZERO                                         
057600                   IF START3 = ZERO                                       
057700                     IF START2 = ZERO                                     
057800                       MOVE MAX6 TO MAX1                                  
057900                     ELSE                                                 
058000                       MOVE MAX6 TO MAX2                                  
058100                       SUBTRACT START2 FROM MAX2                          
058200                     END-IF                                               
058300                   ELSE                                                   
058400                     MOVE MAX6 TO MAX3                                    
058500                     SUBTRACT START3 FROM MAX3                            
058600                   END-IF                                                 
058700                 ELSE                                                     
058800                   MOVE MAX6 TO MAX4                                      
058900                   SUBTRACT START4 FROM MAX4                              
059000                 END-IF                                                   
059100               ELSE                                                       
059200                 MOVE MAX6 TO MAX5                                        
059300                 SUBTRACT START5 FROM MAX5                                
059400               END-IF                                                     
059500               MOVE ZERO TO MAX6                                          
059600             ELSE                                                         
059700               SUBTRACT START6 FROM MAX6                                  
059800             END-IF                                                       
059900                                                                          
060000           WHEN OTHER                                                     
060100             CONTINUE                                                     
060200         END-EVALUATE                                                     
060300                                                                          
060400       WHEN WLISTTYP = EPLUS-TYP                                          
060500                                                                          
060600         EVALUATE TRUE                                                    
060700           WHEN MEMAREA (48:15) = 'EASYTRIEVE PLUS'                       
060800                AND START2 = ZERO                                         
060900             SUBTRACT 1 FROM TAB-IX GIVING START2 MAX1                    
061000                                                                          
061100           WHEN MEMAREA (2:30) = 'CLIST                         '         
061200                AND START3 = ZERO                                         
061300             SUBTRACT 6 FROM TAB-IX GIVING START3 MAX2                    
061400             IF START2 = ZERO                                             
061500               MOVE MAX2 TO MAX1                                          
061600               MOVE ZERO TO MAX2                                          
061700             ELSE                                                         
061800               SUBTRACT START2 FROM MAX2                                  
061900             END-IF                                                       
062000                                                                          
062100           WHEN MEMAREA (31:14) = 'LINKAGE EDITOR'                        
062200                AND START4 = ZERO                                         
062300             SUBTRACT 2 FROM TAB-IX GIVING START4 MAX3                    
062400             IF START3 = ZERO                                             
062500               IF START2 = ZERO                                           
062600                 MOVE MAX3 TO MAX1                                        
062700               ELSE                                                       
062800                 MOVE MAX3 TO MAX2                                        
062900                 SUBTRACT START2 FROM MAX2                                
063000               END-IF                                                     
063100               MOVE ZERO TO MAX3                                          
063200             ELSE                                                         
063300               SUBTRACT START3 FROM MAX3                                  
063400             END-IF                                                       
063500                                                                          
063600           WHEN OTHER                                                     
063700             CONTINUE                                                     
063800         END-EVALUATE                                                     
063900                                                                          
064000       WHEN WLISTTYP = ASM-TYP                                            
064100                                                                          
064200         EVALUATE TRUE                                                    
064300           WHEN MEMAREA (48:26) = 'EXTERNAL SYMBOL DICTIONARY'            
064400                AND START2 = ZERO                                         
064500             SUBTRACT 1 FROM TAB-IX GIVING START2 MAX1                    
064600                                                                          
064700           WHEN MEMAREA (8:13) = ' OBJECT CODE '                          
064800                AND START3 = ZERO                                         
064900             SUBTRACT 2 FROM TAB-IX GIVING START3 MAX2                    
065000             IF START2 = ZERO                                             
065100               MOVE MAX2 TO MAX1                                          
065200               MOVE ZERO TO MAX2                                          
065300             ELSE                                                         
065400               SUBTRACT START2 FROM MAX2                                  
065500             END-IF                                                       
065600                                                                          
065700           WHEN MEMAREA (51:21) = 'RELOCATION DICTIONARY'                 
065800                AND START4 = ZERO                                         
065900             SUBTRACT 1 FROM TAB-IX GIVING START4 MAX3                    
066000             IF START3 = ZERO                                             
066100               IF START2 = ZERO                                           
066200                 MOVE MAX3 TO MAX1                                        
066300               ELSE                                                       
066400                 MOVE MAX3 TO MAX2                                        
066500                 SUBTRACT START2 FROM MAX2                                
066600               END-IF                                                     
066700               MOVE ZERO TO MAX3                                          
066800             ELSE                                                         
066900               SUBTRACT START3 FROM MAX3                                  
067000             END-IF                                                       
067100                                                                          
067200           WHEN MEMAREA (47:15) = 'CROSS REFERENCE'                       
067300                AND START5 = ZERO                                         
067400             SUBTRACT 1 FROM TAB-IX GIVING START5 MAX4                    
067500             IF START4 = ZERO                                             
067600               IF START3 = ZERO                                           
067700                 IF START2 = ZERO                                         
067800                   MOVE MAX4 TO MAX1                                      
067900                 ELSE                                                     
068000                   MOVE MAX4 TO MAX2                                      
068100                   SUBTRACT START2 FROM MAX2                              
068200                 END-IF                                                   
068300               ELSE                                                       
068400                 MOVE MAX4 TO MAX3                                        
068500                 SUBTRACT START3 FROM MAX3                                
068600               END-IF                                                     
068700               MOVE ZERO TO MAX4                                          
068800             ELSE                                                         
068900               SUBTRACT START4 FROM MAX4                                  
069000             END-IF                                                       
069100                                                                          
069200           WHEN MEMAREA (36:26) = 'DIAGNOSTIC CROSS REFERENCE'            
069300                AND START6 = ZERO                                         
069400             SUBTRACT 1 FROM TAB-IX GIVING START6 MAX5                    
069500             IF START5 = ZERO                                             
069600               IF START4 = ZERO                                           
069700                 IF START3 = ZERO                                         
069800                   IF START2 = ZERO                                       
069900                     MOVE MAX5 TO MAX1                                    
070000                   ELSE                                                   
070100                     MOVE MAX5 TO MAX2                                    
070200                     SUBTRACT START2 FROM MAX2                            
070300                   END-IF                                                 
070400                 ELSE                                                     
070500                   MOVE MAX5 TO MAX3                                      
070600                   SUBTRACT START3 FROM MAX3                              
070700                 END-IF                                                   
070800               ELSE                                                       
070900                 MOVE MAX5 TO MAX4                                        
071000                 SUBTRACT START4 FROM MAX4                                
071100               END-IF                                                     
071200               MOVE ZERO TO MAX5                                          
071300             ELSE                                                         
071400               SUBTRACT START5 FROM MAX5                                  
071500             END-IF                                                       
071600                                                                          
071700           WHEN MEMAREA (31:14) = 'LINKAGE EDITOR'                        
071800                AND START7 = ZERO                                         
071900             SUBTRACT 2 FROM TAB-IX GIVING START7 MAX6                    
072000             IF START6 = ZERO                                             
072100               IF START5 = ZERO                                           
072200                 IF START4 = ZERO                                         
072300                   IF START3 = ZERO                                       
072400                     IF START2 = ZERO                                     
072500                       MOVE MAX6 TO MAX1                                  
072600                     ELSE                                                 
072700                       MOVE MAX6 TO MAX2                                  
072800                       SUBTRACT START2 FROM MAX2                          
072900                     END-IF                                               
073000                   ELSE                                                   
073100                     MOVE MAX6 TO MAX3                                    
073200                     SUBTRACT START3 FROM MAX3                            
073300                   END-IF                                                 
073400                 ELSE                                                     
073500                   MOVE MAX6 TO MAX4                                      
073600                   SUBTRACT START4 FROM MAX4                              
073700                 END-IF                                                   
073800               ELSE                                                       
073900                 MOVE MAX6 TO MAX5                                        
074000                 SUBTRACT START5 FROM MAX5                                
074100               END-IF                                                     
074200               MOVE ZERO TO MAX6                                          
074300             ELSE                                                         
074400               SUBTRACT START6 FROM MAX6                                  
074500             END-IF                                                       
074600                                                                          
074700           WHEN OTHER                                                     
074800             CONTINUE                                                     
074900         END-EVALUATE                                                     
075000                                                                          
075100       WHEN WLISTTYP = PSB-TYP                                            
075200                                                                          
075300         EVALUATE TRUE                                                    
075400           WHEN MEMAREA (48:26) = 'EXTERNAL SYMBOL DICTIONARY'            
075500                AND START2 = ZERO                                         
075600             SUBTRACT 1 FROM TAB-IX GIVING START2 MAX1                    
075700                                                                          
075800           WHEN MEMAREA (8:13) = ' OBJECT CODE '                          
075900                AND START3 = ZERO                                         
076000             SUBTRACT 2 FROM TAB-IX GIVING START3 MAX2                    
076100             IF START2 = ZERO                                             
076200               MOVE MAX2 TO MAX1                                          
076300               MOVE ZERO TO MAX2                                          
076400             ELSE                                                         
076500               SUBTRACT START2 FROM MAX2                                  
076600             END-IF                                                       
076700                                                                          
076800           WHEN MEMAREA (51:21) = 'RELOCATION DICTIONARY'                 
076900                AND START4 = ZERO                                         
077000             SUBTRACT 1 FROM TAB-IX GIVING START4 MAX3                    
077100             IF START3 = ZERO                                             
077200               IF START2 = ZERO                                           
077300                 MOVE MAX3 TO MAX1                                        
077400               ELSE                                                       
077500                 MOVE MAX3 TO MAX2                                        
077600                 SUBTRACT START2 FROM MAX2                                
077700               END-IF                                                     
077800               MOVE ZERO TO MAX3                                          
077900             ELSE                                                         
078000               SUBTRACT START3 FROM MAX3                                  
078100             END-IF                                                       
078200                                                                          
078300           WHEN MEMAREA (47:15) = 'CROSS REFERENCE'                       
078400                AND START5 = ZERO                                         
078500             SUBTRACT 1 FROM TAB-IX GIVING START5 MAX4                    
078600             IF START4 = ZERO                                             
078700               IF START3 = ZERO                                           
078800                 IF START2 = ZERO                                         
078900                   MOVE MAX4 TO MAX1                                      
079000                 ELSE                                                     
079100                   MOVE MAX4 TO MAX2                                      
079200                   SUBTRACT START2 FROM MAX2                              
079300                 END-IF                                                   
079400               ELSE                                                       
079500                 MOVE MAX4 TO MAX3                                        
079600                 SUBTRACT START3 FROM MAX3                                
079700               END-IF                                                     
079800               MOVE ZERO TO MAX4                                          
079900             ELSE                                                         
080000               SUBTRACT START4 FROM MAX4                                  
080100             END-IF                                                       
080200                                                                          
080300           WHEN MEMAREA (36:26) = 'DIAGNOSTIC CROSS REFERENCE'            
080400                AND START6 = ZERO                                         
080500             SUBTRACT 1 FROM TAB-IX GIVING START6 MAX5                    
080600             IF START5 = ZERO                                             
080700               IF START4 = ZERO                                           
080800                 IF START3 = ZERO                                         
080900                   IF START2 = ZERO                                       
081000                     MOVE MAX5 TO MAX1                                    
081100                   ELSE                                                   
081200                     MOVE MAX5 TO MAX2                                    
081300                     SUBTRACT START2 FROM MAX2                            
081400                   END-IF                                                 
081500                 ELSE                                                     
081600                   MOVE MAX5 TO MAX3                                      
081700                   SUBTRACT START3 FROM MAX3                              
081800                 END-IF                                                   
081900               ELSE                                                       
082000                 MOVE MAX5 TO MAX4                                        
082100                 SUBTRACT START4 FROM MAX4                                
082200               END-IF                                                     
082300               MOVE ZERO TO MAX5                                          
082400             ELSE                                                         
082500               SUBTRACT START5 FROM MAX5                                  
082600             END-IF                                                       
082700                                                                          
082800           WHEN MEMAREA (31:14) = 'LINKAGE EDITOR'                        
082900                AND START7 = ZERO                                         
083000             SUBTRACT 2 FROM TAB-IX GIVING START7 MAX6                    
083100             IF START6 = ZERO                                             
083200               IF START5 = ZERO                                           
083300                 IF START4 = ZERO                                         
083400                   IF START3 = ZERO                                       
083500                     IF START2 = ZERO                                     
083600                       MOVE MAX6 TO MAX1                                  
083700                     ELSE                                                 
083800                       MOVE MAX6 TO MAX2                                  
083900                       SUBTRACT START2 FROM MAX2                          
084000                     END-IF                                               
084100                   ELSE                                                   
084200                     MOVE MAX6 TO MAX3                                    
084300                     SUBTRACT START3 FROM MAX3                            
084400                   END-IF                                                 
084500                 ELSE                                                     
084600                   MOVE MAX6 TO MAX4                                      
084700                   SUBTRACT START4 FROM MAX4                              
084800                 END-IF                                                   
084900               ELSE                                                       
085000                 MOVE MAX6 TO MAX5                                        
085100                 SUBTRACT START5 FROM MAX5                                
085200               END-IF                                                     
085300               MOVE ZERO TO MAX6                                          
085400             ELSE                                                         
085500               SUBTRACT START6 FROM MAX6                                  
085600             END-IF                                                       
085700                                                                          
085800           WHEN MEMAREA (2:7) = 'DBDMAP '                                 
085900                AND START8 = ZERO                                         
086000             SUBTRACT 1 FROM TAB-IX GIVING START8 MAX7                    
086100             IF START7 = ZERO                                             
086200               IF START6 = ZERO                                           
086300                 IF START5 = ZERO                                         
086400                   IF START4 = ZERO                                       
086500                     IF START3 = ZERO                                     
086600                       IF START2 = ZERO                                   
086700                         MOVE MAX7 TO MAX1                                
086800                       ELSE                                               
086900                         MOVE MAX7 TO MAX2                                
087000                         SUBTRACT START2 FROM MAX2                        
087100                       END-IF                                             
087200                     ELSE                                                 
087300                       MOVE MAX7 TO MAX3                                  
087400                       SUBTRACT START3 FROM MAX3                          
087500                     END-IF                                               
087600                   ELSE                                                   
087700                     MOVE MAX7 TO MAX4                                    
087800                     SUBTRACT START4 FROM MAX4                            
087900                   END-IF                                                 
088000                 ELSE                                                     
088100                   MOVE MAX7 TO MAX5                                      
088200                   SUBTRACT START5 FROM MAX5                              
088300                 END-IF                                                   
088400               ELSE                                                       
088500                 MOVE MAX7 TO MAX6                                        
088600                 SUBTRACT START6 FROM MAX6                                
088700               END-IF                                                     
088800               MOVE ZERO TO MAX7                                          
088900             ELSE                                                         
089000               SUBTRACT START7 FROM MAX7                                  
089100             END-IF                                                       
089200                                                                          
089300           WHEN OTHER                                                     
089400             CONTINUE                                                     
089500         END-EVALUATE                                                     
089600                                                                          
089610       WHEN WLISTTYP = FORMAT-TYP                                         
089620                                                                          
089630         EVALUATE TRUE                                                    
089640           WHEN MEMAREA (2:24) = 'MESSAGE/FORMAT SERVICE P'               
089650                AND START2 = ZERO                                         
089660             SUBTRACT 1 FROM TAB-IX GIVING START2 MAX1                    
089670                                                                          
089680           WHEN MEMAREA (11:18) = 'LPAGE/DPAGE SYMBOL'                    
089690                AND START3 = ZERO                                         
089691             SUBTRACT 1 FROM TAB-IX GIVING START3 MAX2                    
089692             IF START2 = ZERO                                             
089693               MOVE MAX2 TO MAX1                                          
089694               MOVE ZERO TO MAX2                                          
089695             ELSE                                                         
089696               SUBTRACT START2 FROM MAX2                                  
089697             END-IF                                                       
089698                                                                          
089699           WHEN MEMAREA (20:26) = 'FORMAT DESCRIPTOR STRUCTUR'            
089700                AND START4 = ZERO                                         
089701             SUBTRACT 1 FROM TAB-IX GIVING START4 MAX3                    
089702             IF START3 = ZERO                                             
089703               IF START2 = ZERO                                           
089704                 MOVE MAX3 TO MAX1                                        
089705               ELSE                                                       
089706                 MOVE MAX3 TO MAX2                                        
089707                 SUBTRACT START2 FROM MAX2                                
089708               END-IF                                                     
089709               MOVE ZERO TO MAX3                                          
089710             ELSE                                                         
089711               SUBTRACT START3 FROM MAX3                                  
089712             END-IF                                                       
089713                                                                          
089714           WHEN MEMAREA (11:14) = 'DEVICE MAPPING'                        
089715                AND START5 = ZERO                                         
089716             SUBTRACT 1 FROM TAB-IX GIVING START5 MAX4                    
089717             IF START4 = ZERO                                             
089718               IF START3 = ZERO                                           
089719                 IF START2 = ZERO                                         
089720                   MOVE MAX4 TO MAX1                                      
089721                 ELSE                                                     
089722                   MOVE MAX4 TO MAX2                                      
089723                   SUBTRACT START2 FROM MAX2                              
089724                 END-IF                                                   
089725               ELSE                                                       
089726                 MOVE MAX4 TO MAX3                                        
089727                 SUBTRACT START3 FROM MAX3                                
089728               END-IF                                                     
089729               MOVE ZERO TO MAX4                                          
089730             ELSE                                                         
089731               SUBTRACT START4 FROM MAX4                                  
089732             END-IF                                                       
089733                                                                          
089734           WHEN MEMAREA (20:27) = 'MESSAGE DESCRIPTOR STRUCTUR'           
089735                AND START6 = ZERO                                         
089736             SUBTRACT 1 FROM TAB-IX GIVING START6 MAX5                    
089737             IF START5 = ZERO                                             
089738               IF START4 = ZERO                                           
089739                 IF START3 = ZERO                                         
089740                   IF START2 = ZERO                                       
089741                     MOVE MAX5 TO MAX1                                    
089742                   ELSE                                                   
089743                     MOVE MAX5 TO MAX2                                    
089744                     SUBTRACT START2 FROM MAX2                            
089745                   END-IF                                                 
089746                 ELSE                                                     
089747                   MOVE MAX5 TO MAX3                                      
089748                   SUBTRACT START3 FROM MAX3                              
089749                 END-IF                                                   
089750               ELSE                                                       
089751                 MOVE MAX5 TO MAX4                                        
089752                 SUBTRACT START4 FROM MAX4                                
089753               END-IF                                                     
089754               MOVE ZERO TO MAX5                                          
089755             ELSE                                                         
089756               SUBTRACT START5 FROM MAX5                                  
089757             END-IF                                                       
089758                                                                          
089759           WHEN MEMAREA (43:16) = 'IEBCOPY MESSAGES'                      
089760                AND START7 = ZERO                                         
089761             SUBTRACT 2 FROM TAB-IX GIVING START7 MAX6                    
089762             IF START6 = ZERO                                             
089763               IF START5 = ZERO                                           
089764                 IF START4 = ZERO                                         
089765                   IF START3 = ZERO                                       
089766                     IF START2 = ZERO                                     
089767                       MOVE MAX6 TO MAX1                                  
089768                     ELSE                                                 
089769                       MOVE MAX6 TO MAX2                                  
089770                       SUBTRACT START2 FROM MAX2                          
089771                     END-IF                                               
089772                   ELSE                                                   
089773                     MOVE MAX6 TO MAX3                                    
089774                     SUBTRACT START3 FROM MAX3                            
089775                   END-IF                                                 
089776                 ELSE                                                     
089777                   MOVE MAX6 TO MAX4                                      
089778                   SUBTRACT START4 FROM MAX4                              
089779                 END-IF                                                   
089780               ELSE                                                       
089781                 MOVE MAX6 TO MAX5                                        
089782                 SUBTRACT START5 FROM MAX5                                
089783               END-IF                                                     
089784               MOVE ZERO TO MAX6                                          
089785             ELSE                                                         
089786               SUBTRACT START6 FROM MAX6                                  
089787             END-IF                                                       
089823                                                                          
089824           WHEN OTHER                                                     
089825             CONTINUE                                                     
089826         END-EVALUATE                                                     
089827                                                                          
089828       WHEN WLISTTYP = SPACE                                              
089830                                                                          
089900         EVALUATE TRUE                                                    
090000           WHEN MEMAREA (21:8) = '+V12350 '                               
090100             MOVE COBOL-TYP TO WLISTTYP                                   
090200             MOVE COBOL-PANEL TO WPANEL                                   
090300             MOVE '7' TO MAXSECT                                          
090400                                                                          
090500           WHEN MEMAREA (21:8) = '+V12356 '                               
090600             MOVE ASM-TYP TO WLISTTYP                                     
090700             MOVE ASM-PANEL TO WPANEL                                     
090800             MOVE '7' TO MAXSECT                                          
090900                                                                          
091000           WHEN MEMAREA (31:4) = 'EZTP'                                   
091100             MOVE EPLUS-TYP TO WLISTTYP                                   
091200             MOVE EPLUS-PANEL TO WPANEL                                   
091300             MOVE '4' TO MAXSECT                                          
091400                                                                          
091500           WHEN MEMAREA (40:7) = 'MAP    '                                
091600             MOVE PSB-TYP TO WLISTTYP                                     
091700             MOVE PSB-PANEL TO WPANEL                                     
091800             MOVE '8' TO MAXSECT                                          
091900                                                                          
091910           WHEN MEMAREA (40:7) = 'MFSCOPY'                                
091920             MOVE FORMAT-TYP TO WLISTTYP                                  
091930             MOVE FORMAT-PANEL TO WPANEL                                  
091940             MOVE '7' TO MAXSECT                                          
091950                                                                          
092000           WHEN OTHER                                                     
092100             CONTINUE                                                     
092200         END-EVALUATE                                                     
092300                                                                          
092400     END-EVALUATE                                                         
092500     .                                                                    
092600     EJECT                                                                
092700 C-LAGRA-VARIABLER SECTION.                                               
092800     SKIP2                                                                
092900     IF WPANEL = SPACE                                                    
093000       DISPLAY 'KAN INTE AVGÖRA VILKEN SORTS LISTA DET ÄR'                
093100       PERFORM S99-ABEND                                                  
093200     END-IF                                                               
093300                                                                          
093400     MOVE TAB-IX TO MAXTOT                                                
093500                                                                          
093600     IF START8 > ZERO                                                     
093700       SUBTRACT START8 FROM TAB-IX GIVING MAX8                            
093800     END-IF                                                               
093900                                                                          
094000     IF START7 > ZERO AND MAX7 = 0                                        
094100       IF START8 = ZERO                                                   
094200         SUBTRACT START7 FROM TAB-IX GIVING MAX7                          
094300       ELSE                                                               
094400         MOVE START8 TO MAX7                                              
094500       END-IF                                                             
094600     END-IF                                                               
094700                                                                          
094800     IF START6 > ZERO AND MAX6 = 0                                        
094900       IF START7 = ZERO                                                   
095000         SUBTRACT START6 FROM TAB-IX GIVING MAX6                          
095100       ELSE                                                               
095200         MOVE START7 TO MAX6                                              
095300       END-IF                                                             
095400     END-IF                                                               
095500                                                                          
095600     IF START5 > ZERO AND MAX5 = 0                                        
095700       IF START6 = ZERO                                                   
095800         IF START7 = ZERO                                                 
095900           SUBTRACT START5 FROM TAB-IX GIVING MAX5                        
096000         ELSE                                                             
096100           MOVE START7 TO MAX5                                            
096200         END-IF                                                           
096300       ELSE                                                               
096400         MOVE START6 TO MAX5                                              
096500       END-IF                                                             
096600     END-IF                                                               
096700                                                                          
096800     IF START4 > ZERO AND MAX4 = 0                                        
096900       IF START5 = ZERO                                                   
097000         IF START6 = ZERO                                                 
097100           IF START7 = ZERO                                               
097200             SUBTRACT START4 FROM TAB-IX GIVING MAX4                      
097300           ELSE                                                           
097400             MOVE START7 TO MAX4                                          
097500           END-IF                                                         
097600         ELSE                                                             
097700           MOVE START6 TO MAX4                                            
097800         END-IF                                                           
097900       ELSE                                                               
098000         MOVE START5 TO MAX4                                              
098100       END-IF                                                             
098200     END-IF                                                               
098300                                                                          
098400     IF START3 > ZERO AND MAX3 = 0                                        
098500       IF START4 = ZERO                                                   
098600         IF START5 = ZERO                                                 
098700           IF START6 = ZERO                                               
098800             IF START7 = ZERO                                             
098900               SUBTRACT START3 FROM TAB-IX GIVING MAX3                    
099000             ELSE                                                         
099100               MOVE START7 TO MAX3                                        
099200             END-IF                                                       
099300           ELSE                                                           
099400             MOVE START6 TO MAX3                                          
099500           END-IF                                                         
099600         ELSE                                                             
099700           MOVE START5 TO MAX3                                            
099800         END-IF                                                           
099900       ELSE                                                               
100000         MOVE START4 TO MAX3                                              
100100       END-IF                                                             
100200     END-IF                                                               
100300                                                                          
100400     IF START2 > ZERO AND MAX2 = 0                                        
100500       IF START3 = ZERO                                                   
100600         IF START4 = ZERO                                                 
100700           IF START5 = ZERO                                               
100800             IF START6 = ZERO                                             
100900               IF START7 = ZERO                                           
101000                 SUBTRACT START2 FROM TAB-IX GIVING MAX2                  
101100               ELSE                                                       
101200                 MOVE START7 TO MAX2                                      
101300               END-IF                                                     
101400             ELSE                                                         
101500               MOVE START6 TO MAX2                                        
101600             END-IF                                                       
101700           ELSE                                                           
101800             MOVE START5 TO MAX2                                          
101900           END-IF                                                         
102000         ELSE                                                             
102100           MOVE START4 TO MAX2                                            
102200         END-IF                                                           
102300       ELSE                                                               
102400         MOVE START3 TO MAX2                                              
102500       END-IF                                                             
102600     END-IF                                                               
102700                                                                          
102800     CALL ISPLINK USING ISP-VPUT N-START2 ISP-SHARED                      
102900     IF RETURN-CODE > 0                                                   
103000       DISPLAY 'KAN INTE VPUTTA START2'                                   
103100       PERFORM S99-ABEND                                                  
103200     END-IF                                                               
103300                                                                          
103400     CALL ISPLINK USING ISP-VPUT N-START3 ISP-SHARED                      
103500     IF RETURN-CODE > 0                                                   
103600       DISPLAY 'KAN INTE VPUTTA START3'                                   
103700       PERFORM S99-ABEND                                                  
103800     END-IF                                                               
103900                                                                          
104000     CALL ISPLINK USING ISP-VPUT N-START4 ISP-SHARED                      
104100     IF RETURN-CODE > 0                                                   
104200       DISPLAY 'KAN INTE VPUTTA START4'                                   
104300       PERFORM S99-ABEND                                                  
104400     END-IF                                                               
104500                                                                          
104600     CALL ISPLINK USING ISP-VPUT N-START5 ISP-SHARED                      
104700     IF RETURN-CODE > 0                                                   
104800       DISPLAY 'KAN INTE VPUTTA START5'                                   
104900       PERFORM S99-ABEND                                                  
105000     END-IF                                                               
105100                                                                          
105200     CALL ISPLINK USING ISP-VPUT N-START6 ISP-SHARED                      
105300     IF RETURN-CODE > 0                                                   
105400       DISPLAY 'KAN INTE VPUTTA START6'                                   
105500       PERFORM S99-ABEND                                                  
105600     END-IF                                                               
105700                                                                          
105800     CALL ISPLINK USING ISP-VPUT N-START7 ISP-SHARED                      
105900     IF RETURN-CODE > 0                                                   
106000       DISPLAY 'KAN INTE VPUTTA START7'                                   
106100       PERFORM S99-ABEND                                                  
106200     END-IF                                                               
106300                                                                          
106400     CALL ISPLINK USING ISP-VPUT N-START8 ISP-SHARED                      
106500     IF RETURN-CODE > 0                                                   
106600       DISPLAY 'KAN INTE VPUTTA START8'                                   
106700       PERFORM S99-ABEND                                                  
106800     END-IF                                                               
106900                                                                          
107000     CALL ISPLINK USING ISP-VPUT N-MAX1 ISP-SHARED                        
107100     IF RETURN-CODE > 0                                                   
107200       DISPLAY 'KAN INTE VPUTTA MAX1'                                     
107300       PERFORM S99-ABEND                                                  
107400     END-IF                                                               
107500                                                                          
107600     CALL ISPLINK USING ISP-VPUT N-MAX2 ISP-SHARED                        
107700     IF RETURN-CODE > 0                                                   
107800       DISPLAY 'KAN INTE VPUTTA MAX2'                                     
107900       PERFORM S99-ABEND                                                  
108000     END-IF                                                               
108100                                                                          
108200     CALL ISPLINK USING ISP-VPUT N-MAX3 ISP-SHARED                        
108300     IF RETURN-CODE > 0                                                   
108400       DISPLAY 'KAN INTE VPUTTA MAX3'                                     
108500       PERFORM S99-ABEND                                                  
108600     END-IF                                                               
108700                                                                          
108800     CALL ISPLINK USING ISP-VPUT N-MAX4 ISP-SHARED                        
108900     IF RETURN-CODE > 0                                                   
109000       DISPLAY 'KAN INTE VPUTTA MAX4'                                     
109100       PERFORM S99-ABEND                                                  
109200     END-IF                                                               
109300                                                                          
109400     CALL ISPLINK USING ISP-VPUT N-MAX5 ISP-SHARED                        
109500     IF RETURN-CODE > 0                                                   
109600       DISPLAY 'KAN INTE VPUTTA MAX5'                                     
109700       PERFORM S99-ABEND                                                  
109800     END-IF                                                               
109900                                                                          
110000     CALL ISPLINK USING ISP-VPUT N-MAX6 ISP-SHARED                        
110100     IF RETURN-CODE > 0                                                   
110200       DISPLAY 'KAN INTE VPUTTA MAX6'                                     
110300       PERFORM S99-ABEND                                                  
110400     END-IF                                                               
110500                                                                          
110600     CALL ISPLINK USING ISP-VPUT N-MAX7 ISP-SHARED                        
110700     IF RETURN-CODE > 0                                                   
110800       DISPLAY 'KAN INTE VPUTTA MAX7'                                     
110900       PERFORM S99-ABEND                                                  
111000     END-IF                                                               
111100                                                                          
111200     CALL ISPLINK USING ISP-VPUT N-MAX8 ISP-SHARED                        
111300     IF RETURN-CODE > 0                                                   
111400       DISPLAY 'KAN INTE VPUTTA MAX8'                                     
111500       PERFORM S99-ABEND                                                  
111600     END-IF                                                               
111700                                                                          
111800     CALL ISPLINK USING ISP-VPUT N-RADLRECL ISP-SHARED                    
111900     IF RETURN-CODE > 0                                                   
112000       DISPLAY 'KAN INTE VPUTTA RADLRECL'                                 
112100       PERFORM S99-ABEND                                                  
112200     END-IF                                                               
112300                                                                          
112400     CALL ISPLINK USING ISP-VPUT N-MAXSECT ISP-SHARED                     
112500     IF RETURN-CODE > 0                                                   
112600       DISPLAY 'KAN INTE VPUTTA MAXSECT'                                  
112700       PERFORM S99-ABEND                                                  
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 D-NYA-VARIABLER SECTION.                                                 
113200     SKIP2                                                                
113300     CALL ISPLINK USING ISP-VDEFINE N-SECT SECT CHAR                      
113400                         L-SECT VDEFINE-OPT                               
113500     IF RETURN-CODE > 8                                                   
113600       DISPLAY 'KAN INTE VDEFFA SECT'                                     
113700       PERFORM S99-ABEND                                                  
113800     END-IF                                                               
113900                                                                          
114000     CALL ISPLINK USING ISP-VDEFINE N-NYSECT NYSECT CHAR                  
114100                         L-NYSECT VDEFINE-OPT                             
114200     IF RETURN-CODE > 8                                                   
114300       DISPLAY 'KAN INTE VDEFFA NYSECT'                                   
114400       PERFORM S99-ABEND                                                  
114500     END-IF                                                               
114600                                                                          
114700     CALL ISPLINK USING ISP-VDEFINE N-TOPRADNR TOPRADNR PACK              
114800                         L-TOPRADNR VDEFINE-OPT                           
114900     IF RETURN-CODE > 8                                                   
115000       DISPLAY 'KAN INTE VDEFFA TOPRADNR'                                 
115100       PERFORM S99-ABEND                                                  
115200     END-IF                                                               
115300                                                                          
115400     CALL ISPLINK USING ISP-VDEFINE N-MAXRADNR MAXRADNR PACK              
115500                         L-MAXRADNR VDEFINE-OPT                           
115600     IF RETURN-CODE > 8                                                   
115700       DISPLAY 'KAN INTE VDEFFA MAXRADNR'                                 
115800       PERFORM S99-ABEND                                                  
115900     END-IF                                                               
116000                                                                          
116100     MOVE SELSECT TO SECT                                                 
116200     MOVE 'N' TO NYSECT                                                   
116300                                                                          
116400     EVALUATE TRUE                                                        
116500       WHEN SECT = 0                                                      
116600         MOVE ZERO TO TOPRADNR                                            
116700         MOVE MAXTOT TO MAXRADNR                                          
116800                                                                          
116900       WHEN SECT = 1                                                      
117000         MOVE ZERO TO TOPRADNR                                            
117100         MOVE MAX1 TO MAXRADNR                                            
117200                                                                          
117300       WHEN SECT = 2                                                      
117400         MOVE START2 TO TOPRADNR                                          
117500         MOVE MAX2 TO MAXRADNR                                            
117600                                                                          
117700       WHEN SECT = 3                                                      
117800         MOVE START3 TO TOPRADNR                                          
117900         MOVE MAX3 TO MAXRADNR                                            
118000                                                                          
118100       WHEN SECT = 4                                                      
118200         MOVE START4 TO TOPRADNR                                          
118300         MOVE MAX4 TO MAXRADNR                                            
118400                                                                          
118500       WHEN SECT = 5                                                      
118600         MOVE START5 TO TOPRADNR                                          
118700         MOVE MAX5 TO MAXRADNR                                            
118800                                                                          
118900       WHEN SECT = 6                                                      
119000         MOVE START6 TO TOPRADNR                                          
119100         MOVE MAX6 TO MAXRADNR                                            
119200                                                                          
119300       WHEN SECT = 7                                                      
119400         MOVE START7 TO TOPRADNR                                          
119500         MOVE MAX7 TO MAXRADNR                                            
119600                                                                          
119700       WHEN SECT = 8                                                      
119800         MOVE START8 TO TOPRADNR                                          
119900         MOVE MAX8 TO MAXRADNR                                            
120000                                                                          
120100       WHEN OTHER                                                         
120200         DISPLAY 'OKÄND SECTION ' SECT                                    
120300         PERFORM S99-ABEND                                                
120400     END-EVALUATE                                                         
120500                                                                          
120600     CALL ISPLINK USING ISP-VPUT N-SECT ISP-SHARED                        
120700     IF RETURN-CODE > 0                                                   
120800       DISPLAY 'KAN INTE VPUTTA SECT'                                     
120900       PERFORM S99-ABEND                                                  
121000     END-IF                                                               
121100                                                                          
121200     CALL ISPLINK USING ISP-VPUT N-NYSECT ISP-SHARED                      
121300     IF RETURN-CODE > 0                                                   
121400       DISPLAY 'KAN INTE VPUTTA NYSECT'                                   
121500       PERFORM S99-ABEND                                                  
121600     END-IF                                                               
121700                                                                          
121800     CALL ISPLINK USING ISP-VPUT N-TOPRADNR ISP-SHARED                    
121900     IF RETURN-CODE > 0                                                   
122000       DISPLAY 'KAN INTE VPUTTA TOPRADNR'                                 
122100       PERFORM S99-ABEND                                                  
122200     END-IF                                                               
122300                                                                          
122400     CALL ISPLINK USING ISP-VPUT N-MAXRADNR ISP-SHARED                    
122500     IF RETURN-CODE > 0                                                   
122600       DISPLAY 'KAN INTE VPUTTA MAXRADNR'                                 
122700       PERFORM S99-ABEND                                                  
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 E-BROWSA-LISTA  SECTION.                                                 
123200     SKIP2                                                                
123300     CALL W9600210 USING READPTR CMDPTR DATAPTR RADTAB                    
123400                                                                          
123500     CALL ISPLINK USING ISP-CONTROL ISP-ERRORS ISP-RETURN                 
123600     CALL ISPLINK USING ISP-BRIF WDUMMY RECFM LRECL                       
123700           READPTR CMDPTR DATAPTR WDUMMY WDUMMY WDUMMY                    
123800                                                                          
123900     IF RETURN-CODE > 0                                                   
124000       IF RETURN-CODE = 12                                                
124100         CONTINUE                                                         
124200       ELSE                                                               
124300         DISPLAY 'RETURKOD STÖRRE ÄN 12 EFTER BRIF'                       
124400         PERFORM S99-ABEND                                                
124500       END-IF                                                             
124600     END-IF                                                               
124700     CALL ISPLINK USING ISP-CONTROL ISP-ERRORS ISP-CANCEL                 
124800                                                                          
124900     CANCEL W9600220                                                      
125000     CANCEL W9600230                                                      
125100                                                                          
125200     CALL WDELETE USING W9600220                                          
125300     CALL WDELETE USING W9600230                                          
125400                                                                          
125500     CALL ISPLINK USING ISP-VDELETE N-SECT                                
125600     CALL ISPLINK USING ISP-VDELETE N-NYSECT                              
125700     CALL ISPLINK USING ISP-VDELETE N-TOPRADNR                            
125800     CALL ISPLINK USING ISP-VDELETE N-MAXRADNR                            
125900     .                                                                    
126000     EJECT                                                                
126100 Z-FINIT SECTION.                                                         
126200     SKIP2                                                                
126300     CALL ISPLINK USING ISP-VRESET                                        
126400                                                                          
126500     CALL ISPLINK USING LMCLOSE DDVAR                                     
126600     CALL ISPLINK USING LMFREE  DDVAR                                     
126700     .                                                                    
126800     EJECT                                                                
126900 S01-LAES-LISTMEM  SECTION.                                               
127000     SKIP2                                                                
127100     CALL ISPLINK USING LMGET DDVAR ISP-INVAR N-MEMAREA                   
127200                        N-VARLRECL MAXLRECL                               
127300     IF RETURN-CODE > 0                                                   
127400       IF RETURN-CODE = 8                                                 
127500         SET END-OF-LISTMEM TO TRUE                                       
127600       ELSE                                                               
127700         DISPLAY 'LMGET RETURKOD ' RETURN-CODE                            
127800         PERFORM S99-ABEND                                                
127900       END-IF                                                             
128000     END-IF                                                               
128100     .                                                                    
128200 S99-ABEND SECTION.                                                       
128300                                                                          
128400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
128500     .                                                                    
