000400                                                                          
000500 ID  DIVISION.                                                            
000600                                                                          
000700 PROGRAM-ID.    W4883400.                                                 
001100*AUTHOR.        E RINGQVIST.                                              
001200*DATE-WRITTEN.  MAJ 1984.                                                 
001300                                                                          
001500                                                                          
001600*    FUNKTION:                                                            
001700*    PROGRAMMET JÄMFÖR CENTRALA SALDOREGISTRET                            
001800*    MED DET ÖVERSÄNDA LOKALA SALDOREGISTRET.                             
001900*    AVIKELSER SKRIVS UT BÅDE SOM KORRIGERINGSTRANSAR                     
002000*    OCH FELLISTPOSTER.                                                   
002100                                                                          
002200*    INFIL W48833 SORTERAS PÅ ARTIKELNR.                                  
002300*    INFILER W48831 OCH W48833 MATCHAS.                                   
002400*    SALDO ELLER ARTIKELNUMMER AVVIKELSER SKRIVS PÅ FELREGISTER.          
002500*    UTFIL W48835 UPPDATERAR CENTRALA SALDOREGISTRET.                     
002600*    UTFIL W48837 INNEHÅLLER FELPOSTER SOM SKRIVS UT PÅ FELLISTA.         
002700                                                                          
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100*                                                                         
003200 FILE-CONTROL.                                                            
003300                                                                          
003400*--------------------------------------------- INFILER:                   
003500*                                              CENTRALT SALDOREG.         
003600     SELECT W48833                       ASSIGN TO UT-S-W48834D1.         
003700*                                              LOKALT SALDOREG.           
003800     SELECT W48831                       ASSIGN TO UT-S-W48834D2.         
003900                                                                          
004000*--------------------------------------------- UTFILER:                   
004100*                                              ÄNDRINGS TRANSAR           
004200     SELECT W48835                       ASSIGN TO UT-S-W48834D3.         
004300*                                              FELLISTPOSTER              
004400     SELECT W48837                       ASSIGN TO UT-S-W48834D4.         
004500                                                                          
004600*--------------------------------------------- SORTFIL:                   
004700*                                                                         
004800     SELECT SORTFIL                      ASSIGN TO UT-S-W48834DS.         
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100                                                                          
005200 FILE SECTION.                                                            
005300                                                                          
005400 FD  W48833                                                               
005500     RECORDING      F                                                     
005600     BLOCK CONTAINS 0.                                                    
005700                                                                          
005800*01  POST    -COPY W488031    -PRE W48833- -L.                            
006000     EJECT                                                                
006100 FD  W48831                                                               
006200     RECORDING      F                                                     
006300     BLOCK CONTAINS 0.                                                    
006400                                                                          
006500*01  POST    -COPY W488031    -PRE W48831- -L.                            
006700     EJECT                                                                
006800 FD  W48835                                                               
006900     RECORDING      F                                                     
007000     BLOCK CONTAINS 0.                                                    
007100                                                                          
007200*01  POST    -COPY W488031    -PRE W48835- -L.                            
007400     EJECT                                                                
007500 FD  W48837                                                               
007600     RECORDING      F                                                     
007700     BLOCK CONTAINS 0.                                                    
007800                                                                          
007900 01  W48837-POST  -COPY W48837 -L.                                        
008000     EJECT                                                                
008100 SD  SORTFIL                                                              
008200                .                                                         
008300                                                                          
008400*01  POST  -COPY W488031    -PRE SORT-.                                   
008600     EJECT                                                                
008700 WORKING-STORAGE SECTION.                                                 
008701                                                                          
008710*    -- CHECKED BY WY2000                                                 
008800 77  IDPGM                       PIC X(8)    VALUE 'W4883400'.            
009700                                                                          
009800 01  ARBETS-AREOR.                                                        
009900     03  JA                      PIC X(1)    VALUE 'J'.                   
010000     03  NEJ                     PIC X(1)    VALUE 'N'.                   
010100                                                                          
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010400                                                                          
010500     03  ABEND-RKOD              PIC S9(4)   COMP.                        
010600                                                                          
010700     03  W48831-EOF              PIC X(1)    VALUE 'N'.                   
010800     03  SORT-EOF                PIC X(1)    VALUE 'N'.                   
010900                                                                          
011000     03  CEN-ID.                                                          
011100         05 CEN-ARTNR            PIC S9(9)   VALUE ZERO  COMP-3.          
011200         05 CEN-DC               PIC X(2)    VALUE SPACE.                 
011300                                                                          
011400     03  WLOK-ID.                                                         
011500         05 WLOK-ARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
011600         05 WLOK-DC              PIC X(2)    VALUE SPACE.                 
011700     EJECT                                                                
011800*01  -COPY W0005      -PRE POSTSUM-.                                      
012000     EJECT                                                                
012100 01  IN-AREA-START-01            PIC X(16)  VALUE 'IN-01'.                
012200*01  AREA    -COPY W488031    -PRE WSORT-.                                
012400     EJECT                                                                
012500 01  IN-AREA-START-02            PIC X(16)  VALUE 'IN-02'.                
012600*                                                                         
012700*01  AREA    -COPY W488031    -PRE IN-.                                   
012900     EJECT                                                                
013000 01  UT-AREA-START-01            PIC X(16)  VALUE 'UT-01'.                
013100*01  AREA    -COPY W488031    -PRE W48835-.                               
013300     EJECT                                                                
013400 01  UT-AREA-START-02            PIC X(16)  VALUE 'UT-02'.                
013500*                                                                         
013600*01  AREA    -COPY W48837     -PRE W48837-.                               
013800     EJECT                                                                
013900 PROCEDURE DIVISION.                                                      
014000                                                                          
014100     PERFORM A-INIT                                                       
014200                                                                          
014300     PERFORM S01-LAES-W48831                                              
014400     IF W48831-EOF = NEJ                                                  
014500       SORT SORTFIL                                                       
014600            ASCENDING KEY SORT-IDARTNR                                    
014700            USING  W48833                                                 
014800            OUTPUT PROCEDURE B-EFTER-SORT                                 
014900                                                                          
015000       IF SORT-RETURN > ZERO                                              
015100         MOVE SORT-RETURN TO ABEND-RKOD                                   
015200         DISPLAY IDPGM                                                    
015300                ' RETURKOD FRÅN SORTEN = ' ABEND-RKOD                     
015400         UPON CONSOLE                                                     
015500         MOVE 20 TO ABEND-RKOD                                            
015600         CALL ABEND USING ABEND-RKOD                                      
015700       END-IF                                                             
015800     ELSE                                                                 
015900       PERFORM S08-W48831-SAKNAS                                          
016000       PERFORM S04-SKRIV-W48837                                           
016100     END-IF                                                               
016200     PERFORM Z-FINIT                                                      
016300                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     CONTINUE.                                                            
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900                                                                          
017000     OPEN INPUT                                                           
017100         W48831                                                           
017200     OPEN OUTPUT                                                          
017300         W48835                                                           
017400         W48837                                                           
017500                                                                          
017600     MOVE 'W48834' TO POSTSUM-PROGNAMN                                    
017700     CONTINUE.                                                            
017800     EJECT                                                                
017900 B-EFTER-SORT SECTION.                                                    
018000                                                                          
018100*    (FÖRSTA POSTEN PÅ W48831 HAR REDAN LÄSTS I HUVUD-SEKTIONEN)          
018200     PERFORM S02-RETURN                                                   
018300*                                                                         
018400     IF SORT-EOF = JA                                                     
018500       DISPLAY IDPGM ' CENTRAL SALDOFIL (W48833) ÄR TOM.'                 
018600               UPON CONSOLE                                               
018700       DISPLAY '        GICK FÖREGÅENDE JOBB EJ BRA?'                     
018800               UPON CONSOLE                                               
018900       MOVE 16 TO ABEND-RKOD                                              
019000       CALL ABEND USING ABEND-RKOD                                        
019100     ELSE                                                                 
019200       PERFORM UNTIL                                                      
019300        NOT ( W48831-EOF = NEJ OR SORT-EOF = NEJ )                        
019400         IF CEN-ID = WLOK-ID                                              
019500           IF WSORT-KVBUFF-F = IN-KVBUFF-F                                
019600                  AND WSORT-KVBUFF-OF  = IN-KVBUFF-OF                     
019700                  AND WSORT-KVKOLLI-F  = IN-KVKOLLI-F                     
019800                  AND WSORT-KVKOLLI-OF = IN-KVKOLLI-OF                    
019900* - - - - - - - - - - - - - - - - - - - - - - - - - - - SALDO OK          
020000             CONTINUE                                                     
020100           ELSE                                                           
020200             PERFORM S03-SKRIV-W48835                                     
020400**            PERFORM S04-SKRIV-W48837                                    
020500           END-IF                                                         
020600           PERFORM S02-RETURN                                             
020700           PERFORM S01-LAES-W48831                                        
020800         ELSE                                                             
020900           IF CEN-ID < WLOK-ID                                            
021000             IF WSORT-KVBUFF-F > ZERO                                     
021100                    OR WSORT-KVBUFF-OF > ZERO                             
021200* - - - - - - - - - - - - - - - - - - - ARTIKELN SAKNAS PÅ LOK-REG        
021300               PERFORM S07-FLYTTA-ARTSAKNAS-LOK                           
021400               PERFORM S04-SKRIV-W48837                                   
021500             END-IF                                                       
021600             PERFORM S02-RETURN                                           
021700           ELSE                                                           
021800             PERFORM S06-FLYTTA-ARTSAKNAS-CEN                             
021900             PERFORM S04-SKRIV-W48837                                     
022000             PERFORM S01-LAES-W48831                                      
022100           END-IF                                                         
022200         END-IF                                                           
022300       END-PERFORM                                                        
022400     END-IF                                                               
022500     CONTINUE.                                                            
022600     EJECT                                                                
022700 S01-LAES-W48831 SECTION.                                                 
022800                                                                          
022900     READ W48831 INTO IN-AREA                                             
023000         AT END                                                           
023100             MOVE HIGH-VALUE TO WLOK-ID                                   
023200             MOVE JA TO W48831-EOF                                        
023300     END-READ                                                             
023400                                                                          
023500     IF W48831-EOF = NEJ                                                  
023600       MOVE IN-IDARTNR  TO WLOK-ARTNR                                     
023700       MOVE IN-IDDC     TO WLOK-DC                                        
023800       MOVE 'W48831'    TO POSTSUM-FDNAMN                                 
023900       MOVE 'W48834D2'  TO POSTSUM-DDNAMN2                                
024000       MOVE '031'       TO POSTSUM-TRANSTYP                               
024100       CALL POSTSUM USING POSTSUM-PARM                                    
024200     END-IF                                                               
024300     CONTINUE.                                                            
024400     EJECT                                                                
024500 S02-RETURN SECTION.                                                      
024600                                                                          
024700     RETURN SORTFIL  INTO WSORT-AREA                                      
024800         AT END                                                           
024900             MOVE HIGH-VALUE TO CEN-ID                                    
025000             MOVE JA TO SORT-EOF                                          
025100     END-RETURN                                                           
025200                                                                          
025300     IF SORT-EOF = NEJ                                                    
025400       MOVE WSORT-IDARTNR  TO CEN-ARTNR                                   
025500       MOVE WSORT-IDDC     TO CEN-DC                                      
025600       MOVE 'W48833'       TO POSTSUM-FDNAMN                              
025700       MOVE 'W48834DS'     TO POSTSUM-DDNAMN2                             
025800       MOVE '033'          TO POSTSUM-TRANSTYP                            
025900       CALL POSTSUM USING POSTSUM-PARM                                    
026000     END-IF                                                               
026100     CONTINUE.                                                            
026200     EJECT                                                                
026300 S03-SKRIV-W48835 SECTION.                                                
026400                                                                          
026500     MOVE '035'           TO W48835-IDPTYP                                
026600     MOVE WSORT-IDARTNR   TO W48835-IDARTNR                               
026700     MOVE WSORT-IDDC      TO W48835-IDDC                                  
026800     MOVE IN-KVBUFF-F     TO W48835-KVBUFF-F                              
026900     MOVE IN-KVBUFF-OF    TO W48835-KVBUFF-OF                             
027000     MOVE IN-KVKOLLI-F    TO W48835-KVKOLLI-F                             
027100     MOVE IN-KVKOLLI-OF   TO W48835-KVKOLLI-OF                            
027200                                                                          
027300     WRITE W48835-POST FROM W48835-AREA                                   
027400                                                                          
027500     MOVE 'W48835'   TO POSTSUM-FDNAMN                                    
027600     MOVE 'W48834D3' TO POSTSUM-DDNAMN2                                   
027700     MOVE '035'      TO POSTSUM-TRANSTYP                                  
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     CONTINUE.                                                            
028000     EJECT                                                                
028100 S04-SKRIV-W48837 SECTION.                                                
028200                                                                          
028300     WRITE W48837-POST FROM W48837-AREA                                   
028400                                                                          
028500     MOVE 'W48837'      TO POSTSUM-FDNAMN                                 
028600     MOVE 'W48834D4'    TO POSTSUM-DDNAMN2                                
028700     MOVE W48837-IDPTYP TO POSTSUM-TRANSTYP                               
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     CONTINUE.                                                            
029000     EJECT                                                                
030600 S06-FLYTTA-ARTSAKNAS-CEN SECTION.                                        
030700                                                                          
030800     MOVE '002'            TO W48837-IDPTYP                               
030900     MOVE IN-IDARTNR       TO W48837-IDARTNR                              
031000     MOVE IN-IDDC          TO W48837-IDDC                                 
031100     MOVE ZERO             TO W48837-KVBUFF-F-CEN                         
031200                              W48837-KVBUFF-OF-CEN                        
031300                              W48837-KVKOLLI-F-CEN                        
031400                              W48837-KVKOLLI-OF-CEN                       
031500     MOVE IN-KVBUFF-F      TO W48837-KVBUFF-F-LOK                         
031600     MOVE IN-KVBUFF-OF     TO W48837-KVBUFF-OF-LOK                        
031700     MOVE IN-KVKOLLI-F     TO W48837-KVKOLLI-F-LOK                        
031800     MOVE IN-KVKOLLI-OF    TO W48837-KVKOLLI-OF-LOK                       
031900     CONTINUE.                                                            
032000     EJECT                                                                
032100 S07-FLYTTA-ARTSAKNAS-LOK SECTION.                                        
032200                                                                          
032300     MOVE '003'            TO W48837-IDPTYP                               
032400     MOVE WSORT-IDARTNR    TO W48837-IDARTNR                              
032500     MOVE WSORT-IDDC       TO W48837-IDDC                                 
032600     MOVE ZERO             TO W48837-KVBUFF-F-CEN                         
032700                              W48837-KVBUFF-OF-CEN                        
032800                              W48837-KVBUFF-F-LOK                         
032900                              W48837-KVBUFF-OF-LOK                        
033000                              W48837-KVKOLLI-F-CEN                        
033100                              W48837-KVKOLLI-OF-CEN                       
033200                              W48837-KVKOLLI-F-LOK                        
033300                              W48837-KVKOLLI-OF-LOK                       
033400     CONTINUE.                                                            
033500     EJECT                                                                
033600 S08-W48831-SAKNAS SECTION.                                               
033700                                                                          
033800     MOVE '005'            TO W48837-IDPTYP                               
033900     MOVE ZERO             TO W48837-IDARTNR                              
034000                              W48837-IDDC                                 
034100                              W48837-KVBUFF-F-CEN                         
034200                              W48837-KVBUFF-OF-CEN                        
034300                              W48837-KVBUFF-F-LOK                         
034400                              W48837-KVBUFF-OF-LOK                        
034500                              W48837-KVKOLLI-F-CEN                        
034600                              W48837-KVKOLLI-OF-CEN                       
034700                              W48837-KVKOLLI-F-LOK                        
034800                              W48837-KVKOLLI-OF-LOK                       
034900     CONTINUE.                                                            
035000     EJECT                                                                
035100 Z-FINIT SECTION.                                                         
035200                                                                          
035300     CLOSE                                                                
035400         W48831                                                           
035500         W48835                                                           
035600         W48837                                                           
035700                                                                          
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     CONTINUE.                                                            
