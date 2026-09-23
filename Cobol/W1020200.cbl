000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1020200.                                                
000300 AUTHOR.         PETER DAHLÖF.                                            
000400 DATE-WRITTEN.   MAJ 1990.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   TP-UPPDATERINGSPROGRAM.                                  
000800*                STYWINFO FÖR BATCH-PRINTPROGRAM I RUTIN W112S4.          
000900*                STARTAR SOP SOM GÖR ORDER PÅ RUTINEN.                    
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W1T202                                              
001400*        MID:         W1I20201                                            
001500*    UTDATA.                                                              
001600*        MOD:         W1O20201                                            
001700*                     WMSGSOP      OM INDATA RÄTT                         
001800*    SUBPROGRAM.                                                          
001900*        FELLOG                                                           
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400                                                                          
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'W1020200'.            
003100 77  JA                          PIC X(1)    VALUE 'J'.                   
003200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003300 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
003400 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003500                                                                          
003600 77  SPRAAK-KOLL                 PIC X.                                   
003700     88  FINNS                               VALUE 'J'.                   
003800                                                                          
003900 77  WS-IDTRANS                  PIC X(4).                                
004000     88  EGEN-BILD                           VALUE '1202'.                
004100                                                                          
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
004500     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
004600     03  WMEDKONV                PIC X(8)  VALUE 'WMEDKONV'.              
004700     03  W006PRT                 PIC X(8)  VALUE 'W006PRT '.              
004800                                                                          
004900     EJECT                                                                
005000 01  LIST-DATA.                                                           
005100   03  FILLER                    PIC X(5) VALUE 'DATA('.                  
005200   03  -COPY W1127100 -PRE LIST-.                                         
005300   03  FILLER                    PIC X(1) VALUE ')'.                      
005400   03  FILLER                    PIC X(17) VALUE SPACE.                   
005500                                                                          
005600 01  LIST-DEST.                                                           
005700   03  FILLER                    PIC X(5)  VALUE 'DEST('.                 
005800   03  LIST-IDNODE               PIC X(8).                                
005900   03  FILLER                    PIC X(1)  VALUE ')'.                     
006000   03  FILLER                    PIC X(66) VALUE SPACE.                   
006100                                                                          
006200     EJECT                                                                
006300 01  FILLER                      PIC X(8)  VALUE 'W006PRT '.              
006400*01  -COPY W006PRT                                                        
006500                                                                          
006600     EJECT                                                                
006700 01  FILLER                      PIC X(8)  VALUE 'WWLAND04'.              
006800*01  -COPY WWLAND04                                                       
006900                                                                          
007000     EJECT                                                                
007100 01  FILLER                      PIC X(8)  VALUE 'WMEDAREA'.              
007200*01  -COPY WMEDAREA                                                       
007300                                                                          
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE ' SOP      '.          
007600*01  -COPY WMSGSOP                                                        
007700                                                                          
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE ' MFS-WS   '.          
008000                                                                          
008100*01  MID -COPY W1I20201                                                   
008200                                                                          
008300     EJECT                                                                
008400*01  -COPY WMSGAREA                                                       
008500                                                                          
008600     EJECT                                                                
008700*    03  MOD -COPY W1O20201  -RED MSG-AREA.                               
008800                                                                          
008900     EJECT                                                                
009000*01  -COPY WMFSAREA                                                       
009100                                                                          
009200     EJECT                                                                
009300******************************************************************        
009400**                                                                        
009500**       ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600**                                                                        
009700 01  IMS-WS.                                                              
009800     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
009900                                                                          
010000*****                    **** STATUS-KOD FRÅN IMS                         
010100     03  STATUS-WS               PIC X(2).                                
010200         88  SEGMENT-FINNS                   VALUE '  '.                  
010300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
010400                                                                          
010500     03  GODK-STATUSKODER.                                                
010600         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
010700                                                                          
010800     EJECT                                                                
010900*                            IMS FUNKTIONSKODER                           
011000*01  -COPY W0003                                                          
011100                                                                          
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400*01  -COPY W0009     -PRE MSG-                                            
011500                                                                          
011600     EJECT                                                                
011700*01  -COPY W0009     -PRE ALT-                                            
011800                                                                          
011900     EJECT                                                                
012000 PROCEDURE DIVISION USING MSG-PCB ALT-PCB.                                
012100 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
012300                                                                          
012400     PERFORM IMS-GET-MSG                                                  
012500     IF SEGMENT-FINNS                                                     
012600       PERFORM A-INIT                                                     
012700       PERFORM B-KOLLA-INDATA                                             
012800       IF INPUT-RETT = JA                                                 
012900         IF MFS-UPDATE                                                    
013000           PERFORM D-STARTA-SOP-W0T606U                                   
013100           MOVE '101'      TO MED-IDMFSINF                                
013200           CALL WMEDKONV USING MED-WMEDAREA                               
013300           MOVE MED-MFSINF TO MOD-TEMFSINF                                
013400         ELSE                                                             
013500           MOVE '003'      TO MED-IDMFSFEL                                
013600           CALL WMEDKONV USING MED-WMEDAREA                               
013700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
013800         END-IF                                                           
013900       ELSE                                                               
014000         MOVE '001'      TO MED-IDMFSFEL                                  
014100         CALL WMEDKONV USING MED-WMEDAREA                                 
014200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
014300       END-IF                                                             
014400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
014500       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O20201 + 4                      
014600       PERFORM IMS-INSERT-MSG                                             
014700     END-IF                                                               
014800                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015600     IF MSG-DUBBLA-TRANSKODER                                             
015700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I20201                 
015800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
015900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
016000     ELSE                                                                 
016100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I20201                 
016200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
016300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
016400     END-IF                                                               
016500     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
016600     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
016700                                                                          
016800     MOVE LOW-VALUE                       TO MSG-AREA                     
016900     MOVE 'W1O202N1'                      TO MFS-IDMOD                    
017000     MOVE '1202'                          TO MOD-IDTRANS                  
017100     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
017200                                                                          
017300     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
017400                                             MOD-TEMFSINF                 
017500     IF EGEN-BILD                                                         
017600       CONTINUE                                                           
017700     ELSE                                                                 
017800       MOVE '7'                           TO MFS-IDPFK                    
017900     END-IF                                                               
018000     IF ENGLISH-TEXT                                                      
018100       MOVE 'GB '                         TO MED-IDSKYLT                  
018200     ELSE                                                                 
018300       MOVE 'S  '                         TO MED-IDSKYLT                  
018400       MOVE '0'                           TO MFS-KDHUVOMR                 
018500     END-IF                                                               
018600     .                                                                    
018700                                                                          
018800     EJECT                                                                
018900 B-KOLLA-INDATA SECTION.                                                  
019000                                                                          
019100     MOVE JA TO INPUT-RETT                                                
019200     MOVE MID-IDSKYLT TO IDSKYLT                                          
019300     IF GODK-IDSKYLT                                                      
019400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR                      
019500     ELSE                                                                 
019600       MOVE NEJ                  TO INPUT-RETT                            
019700       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-ATTR                      
019800     END-IF                                                               
019900                                                                          
020000     MOVE +1 TO INDX                                                      
020100     PERFORM 6 TIMES                                                      
020200       IF MID-IDARTNR-STR (INDX) NUMERIC                                  
020300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-STR-ATTR (INDX)         
020400       ELSE                                                               
020500         IF MID-IDARTNR-STR (INDX) = ALL '+'                              
020600          MOVE ZERO                   TO MID-IDARTNR-STR (INDX)           
020700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-STR-ATTR (INDX)        
020800         ELSE                                                             
020900           MOVE NEJ TO INPUT-RETT                                         
021000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-STR-ATTR (INDX)         
021100         END-IF                                                           
021200       END-IF                                                             
021300       ADD +1 TO INDX                                                     
021400     END-PERFORM                                                          
021500                                                                          
021600     MOVE 002 TO PRT-KDCALL                                               
021700     MOVE MID-IDNODE TO PRT-IDLTERM                                       
021800     CALL W006PRT USING PRT-W006PRT                                       
021900     IF PRT-KDSVAR = 'R'                                                  
022000       MOVE MID-IDNODE           TO LIST-IDNODE                           
022100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDNODE-ATTR                       
022200     ELSE                                                                 
022300       MOVE NEJ                  TO INPUT-RETT                            
022400       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDNODE-ATTR                       
022500       MOVE 'INVALID PRINTER DESTINATION ' TO MOD-TEMFSINF                
022600     END-IF                                                               
022700     .                                                                    
022800                                                                          
022900     EJECT                                                                
023000 D-STARTA-SOP-W0T606U SECTION.                                            
023100                                                                          
023200     MOVE '1202'              TO MSGSOP-IDTRANS                           
023300     MOVE '1'                 TO MSGSOP-KDMFSFOR                          
023400     MOVE 'W112S4'            TO MSGSOP-IDPROCESS                         
023500     MOVE 'O'                 TO MSGSOP-KDSOPFUNK                         
023600                                                                          
023700     MOVE MID-IDSKYLT         TO LIST-IDSKYLT                             
023800     MOVE MID-IDARTNR-STR (1) TO LIST-IDARTNR1                            
023900     MOVE MID-IDARTNR-STR (2) TO LIST-IDARTNR2                            
024000     MOVE MID-IDARTNR-STR (3) TO LIST-IDARTNR3                            
024100     MOVE MID-IDARTNR-STR (4) TO LIST-IDARTNR4                            
024200     MOVE MID-IDARTNR-STR (5) TO LIST-IDARTNR5                            
024300     MOVE MID-IDARTNR-STR (6) TO LIST-IDARTNR6                            
024400                                                                          
024500     STRING LIST-DATA LIST-DEST                                           
024600            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
024700     PERFORM IMS-INSERT-ALT-MSG                                           
024800     .                                                                    
024900                                                                          
025000     EJECT                                                                
025100 MFS-ROER-EJ-FAELT-UT SECTION.                                            
025200                                                                          
025300     MOVE MFS-ROER-EJ-FAELT            TO MOD-IDSKYLT                     
025400                                          MOD-IDARTNR-STR (1)             
025500                                          MOD-IDARTNR-STR (2)             
025600                                          MOD-IDARTNR-STR (3)             
025700                                          MOD-IDARTNR-STR (4)             
025800                                          MOD-IDARTNR-STR (5)             
025900                                          MOD-IDARTNR-STR (6)             
026000                                          MOD-IDNODE                      
026100     .                                                                    
026200                                                                          
026300     EJECT                                                                
026400 IMS-GET-MSG SECTION.                                                     
026500                                                                          
026600     MOVE '  QC' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
026800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100                                                                          
027200                                                                          
027300 IMS-INSERT-MSG SECTION.                                                  
027400                                                                          
027500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
027600     MOVE SPACE TO GODK-STATUSKODER                                       
027700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
027800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100                                                                          
028200                                                                          
028300 IMS-INSERT-ALT-MSG SECTION.                                              
028400                                                                          
028500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
028600     MOVE SPACE TO GODK-STATUSKODER                                       
028700     CALL CBLTDLI USING ISRT ALT-PCB MSGSOP-WMSGSOP                       
028800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028900     PERFORM IMS-STATUSKONTROLL                                           
029000     .                                                                    
029100                                                                          
029200                                                                          
029300 IMS-STATUSKONTROLL SECTION.                                              
029400     SET STATUS-IX TO 1                                                   
029500     SEARCH GODK-STATUS AT END CALL FELLOG                                
029600        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
029700           CONTINUE                                                       
029800     END-SEARCH                                                           
029900     .                                                                    
