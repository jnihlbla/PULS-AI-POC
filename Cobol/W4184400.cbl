000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4184400.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   JANUARI 2015.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MOTTAGNA RADER DC11    VIA D&P                                   
000900*                                                                         
001000* INFIL W41844 - LEVERANSANMÄRKNINGAR                                     
001100*                                                                         
001200* UTFIL W41844D- ANTAL MOTTAGNARADER PER DISTRIKT TILL D&P                
001300*                                                                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- LEVERANSANMÄRKNINGAR                                       
003000     SELECT W41844                     ASSIGN TO W41844D1.                
003100                                                                          
003200*          --- LISTPOSTER TILL D&P                                        
003300     SELECT W41844D                    ASSIGN TO W41844D2.                
003400                                                                          
003500                                                                          
003600 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W41844                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004400*01  -COPY W4184411    -L.                                                
004401                                                                          
004410*01  -COPY W4184401    -L.                                                
004600                                                                          
004700 FD  W41844D                                                              
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000 01  DOP-POST  PIC X(100).                                                
005100                                                                          
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005403 77  IDPGM                       PIC X(8)    VALUE 'W4184400'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
006200                                                                          
006300 77  W41844-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W41844                       VALUE 'J'.                   
006500                                                                          
006600                                                                          
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300                                                                          
007402 77  WS-VALID-KDANMORS           PIC X(2).                                
007500     88 VALID-KDANMORS           VALUE '12' '22' '27' '42'                
007600                                       '52' '54' '62' '72'                
007700                                       '74' '75' '82' '92'                
007800                                       '94' '98'.                         
007900     88 VALID-KDANMORS-98        VALUE '98'.                              
008000                                                                          
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200                                                                          
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008510     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008600                                                                          
008700                                                                          
008800*    --- PARAMETRAR TILL ABEND                                            
008900                                                                          
009400 01  HJAELP-FAELT.                                                        
009500     03  CURR-IDLEVANM.                                                   
009602         05 CURR-IDDISTR         PIC S9(5)   COMP-3 VALUE ZERO.           
009702         05 CURR-IDKUNDNR        PIC S9(7)   COMP-3 VALUE ZERO.           
009802         05 CURR-IDRAPPNR        PIC  9(7)          VALUE ZERO.           
009900                                                                          
010000     03  W-KVRADER               PIC 9(4)    VALUE ZERO.                  
010100     03  W-KVRADER-98            PIC 9(4)    VALUE ZERO.                  
010200     03  W-KVRADER-TOT           PIC 9(4)    VALUE ZERO.                  
010600                                                                          
010700 01  FELTEXT.                                                             
010800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011000                                                                          
011100                                                                          
011200 01  LIST-AREA.                                                           
011300     03  LIST-BLANKRAD           PIC X       VALUE SPACE.                 
011400                                                                          
013300     03  LIST-RUBRIK.                                                     
013400         05  FILLER              PIC X(28)   VALUE                        
013500            ' Received return lines, CDC '.                               
013900         05  LIST-DATUM          PIC 9(6)    VALUE ZERO.                  
014000                                                                          
015700                                                                          
015800     03  LIST-RADRUBRIK.                                                  
015900         05  FILLER              PIC X(10)   VALUE                        
016000            ' District '.                                                 
016100         05  FILLER              PIC X(03)   VALUE SPACE.                 
016300         05  FILLER              PIC X(12)   VALUE                        
016400            'No. of lines'.                                               
016500         05  FILLER              PIC X(03)   VALUE SPACE.                 
016600         05  FILLER              PIC X(22)   VALUE                        
016700            'No. of lines (code 98)'.                                     
016800         05  FILLER              PIC X(03)   VALUE SPACE.                 
016900         05  FILLER              PIC X(18)   VALUE                        
017000            'Total no. of lines'.                                         
026800                                                                          
026900     03  LIST-RAD.                                                        
027000         05  FILLER              PIC X(01)   VALUE SPACE.                 
027100         05  LIST-IDDISTR        PIC Z(4)    VALUE ZERO.                  
027200         05  FILLER              PIC X(16)   VALUE SPACE.                 
027300         05  LIST-KVRADER        PIC Z(3)9   VALUE ZERO.                  
027500         05  FILLER              PIC X(21)   VALUE SPACE.                 
027600         05  LIST-KVRADER-98     PIC Z(3)9   VALUE ZERO.                  
027700         05  FILLER              PIC X(17)   VALUE SPACE.                 
027710         05  LIST-KVRADER-TOT    PIC Z(3)9   VALUE ZERO.                  
027800                                                                          
027900 01  W001-DAP.                                                            
028000     03  FILLER                  PIC X(165)  VALUE SPACE.                 
028100*    --- PARAMETRAR TILL POSTSUM                                          
028200                                                                          
028300*01  -COPY W0005   -PRE  POSTSUM-                                         
028400                                                                          
028410                                                                          
028420*    --- PARAMETRAR TILL DATKORT                                          
028430*                                                                         
028440 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41844'.              
028450                                                                          
028460 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
028470                                                                          
028480*01  -COPY WDATKORT                                                       
028500                                                                          
028600 01  W41844-AREA-START           PIC X(24)   VALUE                        
028700                                 'W41844-AREA-START  '.                   
028800                                                                          
028900 01  W41844-AREA.                                                         
029000     03  W41844-AREA-0.                                                   
029100       05  FILLER                PIC X(2000).                             
029200*   03  FILLER -COPY W4184401  -PRE 201-  -RED  W41844-AREA-0.            
029300*   03  FILLER -COPY W4184411  -PRE 211-  -RED  W41844-AREA-0.            
029400     EJECT                                                                
039500 PROCEDURE DIVISION.                                                      
039700 MAIN SECTION.                                                            
040000                                                                          
040100     PERFORM A-INIT                                                       
040210     PERFORM S01-LAES-W41844                                              
040220     PERFORM B-KOLLA-RATT-DATUM                                           
040300                                                                          
040400     PERFORM UNTIL END-OF-W41844                                          
040500                                                                          
040600       IF 211-IDLEVANM NOT = CURR-IDLEVANM                                
040700         PERFORM C-NY-IDLEVANM                                            
040800       END-IF                                                             
040810                                                                          
040900       MOVE 211-KDANMORS TO WS-VALID-KDANMORS                             
042500       IF VALID-KDANMORS AND                                              
042510          211-FLANNULL = NEJ                                              
042600         ADD 1   TO W-KVRADER-TOT                                         
042610         IF VALID-KDANMORS-98                                             
042620           ADD 1 TO W-KVRADER-98                                          
042630         ELSE                                                             
042640           ADD 1 TO W-KVRADER                                             
042650         END-IF                                                           
042700       END-IF                                                             
043002                                                                          
043003       IF NOT END-OF-W41844                                               
043100         PERFORM S01-LAES-W41844                                          
043110         IF 201-IDPTYP = '201'                                            
043111           PERFORM B-KOLLA-RATT-DATUM                                     
043120         END-IF                                                           
043200       END-IF                                                             
043300                                                                          
043400     END-PERFORM                                                          
043401                                                                          
043406     IF W-KVRADER-TOT > ZERO                                              
043410       PERFORM S02-SKRIV-LISTRAD                                          
043420     END-IF                                                               
043500                                                                          
043600                                                                          
043700     PERFORM Z-FINIT                                                      
043800                                                                          
043900     MOVE ZERO TO RETURN-CODE                                             
044000     GOBACK                                                               
044100     .                                                                    
044200     EJECT                                                                
044400 A-INIT SECTION.                                                          
044500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
044600                                                                          
044700     OPEN INPUT  W41844                                                   
044800     OPEN OUTPUT W41844D                                                  
044810                                                                          
044820     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
044830     MOVE D-AAR                           TO DAGENS-DATUM-AAR             
044840     MOVE D-MAANAD                        TO DAGENS-DATUM-MAANAD          
044850     MOVE D-DAG                           TO DAGENS-DATUM-DAG             
044900                                                                          
045000**** ACCEPT DAGENS-DATUM  FROM DATE                                       
045010**** MOVE 080709 TO DAGENS-DATUM                                          
045020                                                                          
045100     MOVE   DAGENS-DATUM    TO LIST-DATUM                                 
045200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
045300                                                                          
045400     PERFORM AA-INITIERA-LISTA                                            
045500     .                                                                    
045600     EJECT                                                                
045800 AA-INITIERA-LISTA SECTION.                                               
045900     MOVE 'AA-INIT-LISTA   ' TO CURRENT-SECTION                           
046000                                                                          
046100     MOVE '¤DAPRECLINES '  TO W001-DAP                                    
046200     WRITE DOP-POST FROM W001-DAP                                         
046300                                                                          
046400     MOVE SPACE            TO W001-DAP                                    
046500*    IF RAD-IDANSTNR = 'BP8BA'                                            
046600*      STRING '¤DAP' RAD-IDANSTNR                                         
046700*      DELIMITED BY SIZE INTO W001-DAP                                    
046800*    ELSE                                                                 
046900*      STRING '¤DAP' 'VXC0259'                                            
047000*      DELIMITED BY SIZE INTO W001-DAP                                    
047100*    END-IF                                                               
047200     MOVE '¤DAPRECLINES '  TO W001-DAP                                    
047300     WRITE DOP-POST FROM W001-DAP                                         
047400                                                                          
048800     WRITE DOP-POST FROM LIST-RUBRIK                                      
048900     WRITE DOP-POST FROM LIST-BLANKRAD                                    
049002     WRITE DOP-POST FROM LIST-RADRUBRIK                                   
050100     .                                                                    
050200     EJECT                                                                
050400 B-KOLLA-RATT-DATUM SECTION.                                              
050500     MOVE 'B-KOLLA-DATUM   ' TO CURRENT-SECTION                           
050510                                                                          
050540     PERFORM UNTIL END-OF-W41844 OR                                       
050551         (201-IDPTYP = '201' AND 201-DARETANK(3:6) = DAGENS-DATUM)        
050552                                                                          
050560       PERFORM S01-LAES-W41844                                            
050570     END-PERFORM                                                          
050571                                                                          
050572     IF NOT END-OF-W41844                                                 
050580       PERFORM S01-LAES-W41844                                            
050590     END-IF                                                               
050610     .                                                                    
050620     EJECT                                                                
050640 C-NY-IDLEVANM   SECTION.                                                 
050650     MOVE 'C-NY-IDLEVANM   ' TO CURRENT-SECTION                           
050660                                                                          
050700     IF 201-IDDISTR NOT = CURR-IDDISTR                                    
050800       IF CURR-IDDISTR > ZERO                                             
050810          IF W-KVRADER-TOT > ZERO                                         
050900             PERFORM S02-SKRIV-LISTRAD                                    
050910          END-IF                                                          
051000          MOVE ZERO    TO W-KVRADER                                       
051100          MOVE ZERO    TO W-KVRADER-98                                    
051110          MOVE ZERO    TO W-KVRADER-TOT                                   
051200       END-IF                                                             
051300     END-IF                                                               
051400                                                                          
051600     MOVE 201-IDLEVANM TO CURR-IDLEVANM                                   
052300     .                                                                    
052400     EJECT                                                                
066700 Z-FINIT SECTION.                                                         
066800     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
066900                                                                          
067000     CLOSE W41844 W41844D                                                 
067100                                                                          
067200     MOVE 'S' TO POSTSUM-OPKOD                                            
067300     CALL POSTSUM USING POSTSUM-PARM                                      
067400     .                                                                    
067500     EJECT                                                                
067700 S01-LAES-W41844  SECTION.                                                
067800                                                                          
067900     READ W41844 INTO W41844-AREA                                         
068000     AT END                                                               
068200       SET END-OF-W41844 TO TRUE                                          
068300                                                                          
068400     NOT AT END                                                           
068500       MOVE 'W41844'     TO POSTSUM-FDNAMN                                
068600       MOVE 'W41844D1'   TO POSTSUM-DDNAMN2                               
068702       MOVE 201-IDPTYP   TO POSTSUM-TRANSTYP                              
068800       CALL POSTSUM USING POSTSUM-PARM                                    
068900     END-READ                                                             
069000     .                                                                    
069010     EJECT                                                                
069100 S02-SKRIV-LISTRAD SECTION.                                               
069200     MOVE 'S02-SKRIV-RAD   ' TO CURRENT-SECTION                           
069300                                                                          
069400     MOVE CURR-IDDISTR  TO LIST-IDDISTR                                   
069500     MOVE W-KVRADER     TO LIST-KVRADER                                   
069600     MOVE W-KVRADER-98  TO LIST-KVRADER-98                                
069610     MOVE W-KVRADER-TOT TO LIST-KVRADER-TOT                               
069700     WRITE DOP-POST  FROM  LIST-RAD                                       
069800     .                                                                    
