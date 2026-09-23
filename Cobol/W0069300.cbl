000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0069300.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   JANUARI 1991.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        DISPATCHER (POSTEN, TRANSKÖ-DATABASE).                           
000900*        SÄNDER TRANSAR TILL BERÖRDA PROGRAM FRÅN TRANSKÖN,               
001000*        VID OK-SVAR TAS TRANSEN BORT OCH NÄSTA SÄNDS                     
001100*        VID FEL-SVAR SÄNDS ETT EMAIL VIA WZ01SEND/D&P                    
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W0T693X                                             
001500*        MID:         WMSGKOM                                             
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         TRANS-MID + WMSGKOM                                 
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                   PIC X(8)    VALUE 'W0069300'.                
002900 77  W-COMPILED              PIC X(16)   VALUE SPACE.                     
003000 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
003100 77  JA                      PIC X       VALUE 'J'.                       
003200 77  NEJ                     PIC X       VALUE 'N'.                       
003300 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003400 77  WS-IDOUTREC             PIC X(30)   VALUE SPACE.                     
003500 77  KDRC-DISPLAY            PIC Z(5)    VALUE ZERO.                      
003600 77  RKOD-ABEND-WITH-DUMP    PIC S9(4)   VALUE +33 COMP SYNC.             
003700                                                                          
003800 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
003900   88  NYCKLAR-OK                        VALUE 'J'.                       
004000   88  NYCKLAR-FEL                       VALUE 'N'.                       
004100                                                                          
004200 01  W-VIMSID.                                                            
004300   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004400   03  FILLER                PIC X(4)    VALUE SPACE.                     
004500     EJECT                                                                
004600                                                                          
004700 01  HDR-AREA.                                                            
004800*    03  -COPY WZ01REQU                                                   
004900*    03  -COPY WZ04HDR                                                    
005000     EJECT                                                                
005100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
005200 01  SEND-AREA.                                                           
005300*    03  -COPY WZ01SEND                                                   
005400     EJECT                                                                
005500 01  SEND-RAD.                                                            
005600   03  STYRTECKEN-RAD          PIC X.                                     
005700   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
005800*    --- CONTROL CHARACTERS                                               
005900 01  WS-SKIP1                    PIC X       VALUE ' '.                   
006000 01  WS-SKIP2                    PIC X       VALUE '0'.                   
006100 01  WS-SKIP3                    PIC X       VALUE '-'.                   
006200 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
006300     EJECT                                                                
006400                                                                          
006500 01  GENERELLA-SUBPROGRAM.                                                
006600   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006700   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
006800   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006900   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
007000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007100   03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.                
007200     EJECT                                                                
007300*   -COPY WMEDAREA                                                        
007400     EJECT                                                                
007500*                                                                         
007600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007700*                                                                         
007800 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
007900                                                                          
008000                                                                          
008100*01  MID -COPY WMSGKOM                                                    
008200     EJECT                                                                
008300*01  -COPY WMSGAREA                                                       
008400     EJECT                                                                
008500*                                                                         
008600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700*                                                                         
008800 01  IMS-WS.                                                              
008900   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
009000                                                                          
009100*                        **** STATUS-KOD FRÅN IMS                         
009200   03  STATUS-WS             PIC XX.                                      
009300     88  STATUS-OK                       VALUE '  '.                      
009400     88  SEGMENT-FINNS                   VALUE '  '.                      
009500     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
009600                                                                          
009700                                                                          
009800   03  GODK-STATUSKODER.                                                  
009900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000                                                                          
010100                                                                          
010200 01  NYCKLAR-TILL-DLI.                                                    
010300   03  W-WDP801KY-X.                                                      
010400     05  W-IDSNDNOD          PIC X(8)    VALUE SPACE.                     
010500     05  W-IDSNDJOB          PIC X(8)    VALUE SPACE.                     
010600     05  W-TIREGDAT          PIC S9(7)   VALUE ZERO  COMP-3.              
010700     05  W-TIKLOCK           PIC S9(9)   VALUE ZERO  COMP-3.              
010800   03  W-IDRADNR-X.                                                       
010900     05  W-IDRADNR           PIC S9(5)   VALUE ZERO  COMP-3.              
011000                                                                          
011100                                                                          
011200 01    SSA1                  PIC X(64).                                   
011300 01    SSA2                  PIC X(64).                                   
011400     EJECT                                                                
011500*                            IMS FUNKTIONSKODER                           
011600*01    -COPY W0003                                                        
011700     EJECT                                                                
011800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDP801'.             
011900                                                                          
012000 01  DLI-IO-KOMA01.                                                       
012100*  03  -COPY WDP801  -PRE KOMA-.                                          
012200     EJECT                                                                
012300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDP811'.             
012400                                                                          
012500 01  DLI-IO-KOMA11.                                                       
012600*  03  -COPY WDP811  -PRE KOMA-.                                          
012700     07  FILLER REDEFINES KOMA-TRAN-TRANSDATA.                            
012800       09  KOMA-TRAN-KDTRANS   PIC X(8).                                  
012900       09  KOMA-TRAN-IDTRANS   PIC X(4).                                  
013000       09  KOMA-TRAN-KDSPRAK   PIC X(1).                                  
013100       09  KOMA-TRAN-INDATA    PIC X(987).                                
013200*      09  -COPY W4I25101 -RED KOMA-TRAN-INDATA -PRE 4251-.               
013300*      09  -COPY W4I25201 -RED KOMA-TRAN-INDATA -PRE 4252-.               
013400*      09  -COPY W4I25401 -RED KOMA-TRAN-INDATA -PRE 4254-.               
013500*      09  -COPY W4I25801 -RED KOMA-TRAN-INDATA -PRE 4258-.               
013600*      09  -COPY W4I35501 -RED KOMA-TRAN-INDATA -PRE 4355-.               
013700*      09  -COPY W4I36001 -RED KOMA-TRAN-INDATA -PRE 4360-.               
013800*      09  -COPY W4I39001 -RED KOMA-TRAN-INDATA -PRE 4390-.               
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009     -PRE MSG-                                            
014200                                                                          
014300*01  -COPY W0009     -PRE DISTRDOC-                                       
014400     EJECT                                                                
014500*01  -COPY W0009     -PRE ALT-                                            
014600     EJECT                                                                
014700*01  -COPY W0009     -PRE ALT1112-                                        
014800                                                                          
014900     EJECT                                                                
015000*01  -COPY W0009     -PRE ALT1113-                                        
015100                                                                          
015200     EJECT                                                                
015300*01  -COPY W0009     -PRE ALT1116-                                        
015400                                                                          
015500     EJECT                                                                
015600*01  -COPY W0009     -PRE ALT1117-                                        
015700                                                                          
015800     EJECT                                                                
015900*01  -COPY W0009     -PRE ALT2109-                                        
016000                                                                          
016100     EJECT                                                                
016200*01  -COPY W0009     -PRE ALT2133-                                        
016300                                                                          
016400*01  -COPY W0009     -PRE ALT2163-                                        
016500                                                                          
016600     EJECT                                                                
016700*01  -COPY W0009     -PRE ALT2191-                                        
016800                                                                          
016900     EJECT                                                                
017000*01  -COPY W0009     -PRE ALT2333-                                        
017100                                                                          
017200     EJECT                                                                
017300*01  -COPY W0009     -PRE ALT2335-                                        
017400                                                                          
017500     EJECT                                                                
017600*01  -COPY W0009     -PRE ALT2337-                                        
017700                                                                          
017800*01  -COPY W0009     -PRE ALT2403-                                        
017900                                                                          
018000     EJECT                                                                
018100*01  -COPY W0009     -PRE ALT3151-                                        
018200                                                                          
018300     EJECT                                                                
018400*01  -COPY W0009     -PRE ALT4251-                                        
018500                                                                          
018600*01  -COPY W0009     -PRE ALT4252-                                        
018700                                                                          
018800     EJECT                                                                
018900*01  -COPY W0009     -PRE ALT4252Y-                                       
019000                                                                          
019100     EJECT                                                                
019200*01  -COPY W0009     -PRE ALT4253-                                        
019300                                                                          
019400*01  -COPY W0009     -PRE ALT4254-                                        
019500                                                                          
019600     EJECT                                                                
019700*01  -COPY W0009     -PRE ALT4255-                                        
019800                                                                          
019900*01  -COPY W0009     -PRE ALT4256-                                        
020000                                                                          
020100*01  -COPY W0009     -PRE ALT4258-                                        
020200                                                                          
020300     EJECT                                                                
020400*01  -COPY W0009     -PRE ALT4261-                                        
020500                                                                          
020600*01  -COPY W0009     -PRE ALT4262-                                        
020700                                                                          
020800     EJECT                                                                
020900*01  -COPY W0009     -PRE ALT4263-                                        
021000                                                                          
021100     EJECT                                                                
021200*01  -COPY W0009     -PRE ALT4355-                                        
021300                                                                          
021400     EJECT                                                                
021500*01  -COPY W0009     -PRE ALT4360-                                        
021600                                                                          
021700     EJECT                                                                
021800*01  -COPY W0009     -PRE ALT4390-                                        
021900                                                                          
022000     EJECT                                                                
022100*01  -COPY W0009     -PRE ALT4399-                                        
022200                                                                          
022300     EJECT                                                                
022400*01  -COPY W0009     -PRE ALT4723-                                        
022500     EJECT                                                                
022600*01  -COPY W0009     -PRE ALT4791-                                        
022700                                                                          
022800*01  -COPY W0009     -PRE ALT4792-                                        
022900                                                                          
023000     EJECT                                                                
023100*01  -COPY W0009     -PRE ALT4797-                                        
023200                                                                          
023300     EJECT                                                                
023400*01  -COPY W0009     -PRE ALT5111-                                        
023500                                                                          
023600*01  -COPY W0009     -PRE ALT5112-                                        
023700                                                                          
023800*01  -COPY W0009     -PRE ALT5119-                                        
023900                                                                          
024000*01  -COPY W0009     -PRE ALT5206-                                        
024100                                                                          
024200     EJECT                                                                
024300*01  -COPY W0009     -PRE ALT611B-                                        
024400                                                                          
024500*01  -COPY W0009     -PRE ALT611C-                                        
024600                                                                          
024700     EJECT                                                                
024800*01  -COPY W0009     -PRE ALT611D-                                        
024900                                                                          
025000*01  -COPY W0009     -PRE ALT6192-                                        
025100                                                                          
025200     EJECT                                                                
025300*01  -COPY W0009     -PRE ALT6193-                                        
025400                                                                          
025500*01  -COPY W0009     -PRE ALT619B-                                        
025600                                                                          
025700*01  -COPY W0009     -PRE ALTZ430-                                        
025800                                                                          
025900     EJECT                                                                
026000*01  -COPY W0008     -PRE KOMA-                                           
026100     05  FILLER              PIC X.                                       
026200                                                                          
026300     EJECT                                                                
026400 PROCEDURE DIVISION USING MSG-PCB DISTRDOC-PCB                            
026500                          ALT-PCB                                         
026600                          ALT1112-PCB ALT1113-PCB ALT1116-PCB             
026700                          ALT1117-PCB ALT2109-PCB                         
026800                          ALT2133-PCB ALT2163-PCB ALT2191-PCB             
026900                          ALT2333-PCB ALT2335-PCB                         
027000                          ALT2337-PCB ALT2403-PCB ALT3151-PCB             
027100                          ALT4251-PCB ALT4252-PCB ALT4252Y-PCB            
027200                          ALT4253-PCB ALT4254-PCB ALT4255-PCB             
027300                          ALT4256-PCB ALT4258-PCB                         
027400                          ALT4261-PCB ALT4262-PCB                         
027500                          ALT4263-PCB                                     
027600                          ALT4355-PCB ALT4360-PCB ALT4390-PCB             
027700                          ALT4399-PCB                                     
027800                          ALT4723-PCB ALT4791-PCB ALT4792-PCB             
027900                          ALT4797-PCB                                     
028000                          ALT5111-PCB ALT5112-PCB                         
028100                          ALT5119-PCB ALT5206-PCB                         
028200                          ALT611B-PCB ALT611C-PCB                         
028300                          ALT611D-PCB                                     
028400                          ALT6192-PCB ALT6193-PCB                         
028500                          ALT619B-PCB ALTZ430-PCB KOMA-PCB.               
028600 STYR SECTION.                                                            
028700     EJECT                                                                
028800     PERFORM IMS-GET-MSG                                                  
028900     IF SEGMENT-FINNS                                                     
029000       PERFORM A-INIT                                                     
029100       PERFORM B-KOLLA-NYCKLAR                                            
029200       IF NYCKLAR-OK                                                      
029300         PERFORM IMS-GET-KOMA-ROT                                         
029400         IF SEGMENT-FINNS                                                 
029500           PERFORM IMS-GET-KOMA-TRANS                                     
029600           IF SEGMENT-FINNS                                               
029700             EVALUATE MSG-KOM-IDMFSMED                                    
029800               WHEN '   ' PERFORM C-FIRST-TRANS                           
029900               WHEN '101' PERFORM D-NEXT-TRANS                            
030000               WHEN '249' PERFORM E-SAMMA-TRANS                           
030100               WHEN '114' PERFORM F-GODK-FEL-TRANS                        
030200               WHEN OTHER PERFORM G-FEL-TRANS                             
030300             END-EVALUATE                                                 
030400           ELSE                                                           
030500             MOVE '078' TO MED-IDMFSINF                                   
030600             PERFORM S01-SEND-MAIL                                        
030700           END-IF                                                         
030800         ELSE                                                             
030900           MOVE '010' TO MED-IDMFSINF                                     
031000           PERFORM S01-SEND-MAIL                                          
031100         END-IF                                                           
031200       ELSE                                                               
031300         MOVE '020' TO MED-IDMFSINF                                       
031400         PERFORM S01-SEND-MAIL                                            
031500       END-IF                                                             
031600     END-IF                                                               
031700                                                                          
031800     MOVE ZERO TO RETURN-CODE                                             
031900     GOBACK                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 A-INIT SECTION.                                                          
032300                                                                          
032400     MOVE WHEN-COMPILED TO W-COMPILED                                     
032500                                                                          
032600     MOVE MSG-IO-AREA TO MSG-KOM-WMSGKOM                                  
032700                                                                          
032800     MOVE 'GB ' TO MED-IDSKYLT                                            
032900                                                                          
033000     CALL VIMSID            USING W-IMSID                                 
033100     IF W-IMSID(1:3) = 'IMG'                                              
033200       MOVE 'QASE' TO W-IMSID                                             
033300     END-IF                                                               
033400     IF W-IMSID(1:3) = 'IMP'                                              
033500       MOVE 'DEVE' TO W-IMSID                                             
033600     END-IF                                                               
033700     IF W-IMSID(1:3) = 'IMY'                                              
033800       MOVE 'IGRT' TO W-IMSID                                             
033900     END-IF                                                               
034000     IF W-IMSID(1:3) = 'IMD'                                              
034100       MOVE 'XDEV' TO W-IMSID                                             
034200     END-IF                                                               
034300     IF W-IMSID(1:3) = 'IMB'                                              
034400       MOVE 'ACPT' TO W-IMSID                                             
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 B-KOLLA-NYCKLAR SECTION.                                                 
034900                                                                          
035000     MOVE JA TO NYCKLAR-SW                                                
035100     MOVE MSG-KOM-IDSNDNOD TO W-IDSNDNOD                                  
035200     MOVE MSG-KOM-IDSNDJOB TO W-IDSNDJOB                                  
035300                                                                          
035400     IF MSG-KOM-TIREGDAT NUMERIC                                          
035500       MOVE MSG-KOM-TIREGDAT TO W-TIREGDAT                                
035600     ELSE                                                                 
035700       MOVE NEJ TO NYCKLAR-SW                                             
035800     END-IF                                                               
035900                                                                          
036000     IF MSG-KOM-TIKLOCK NUMERIC                                           
036100       MOVE MSG-KOM-TIKLOCK TO W-TIKLOCK                                  
036200     ELSE                                                                 
036300       MOVE NEJ TO NYCKLAR-SW                                             
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 C-FIRST-TRANS SECTION.                                                   
036800                                                                          
036900     IF KOMA-KOM-KDKOMSTA = 'K'                                           
037000       PERFORM S02-ISRT-ALTMSG                                            
037100       PERFORM IMS-GET-KOMA-ROT                                           
037200       IF MSG-KOM-IDMFSMED NOT = SPACE                                    
037300         MOVE 'F' TO KOMA-KOM-KDKOMSTA                                    
037400         MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                       
037500       ELSE                                                               
037600        MOVE 'S' TO KOMA-KOM-KDKOMSTA                                     
037700       END-IF                                                             
037800       PERFORM IMS-REPL-KOMA                                              
037900     ELSE                                                                 
038000       MOVE '079' TO MED-IDMFSINF                                         
038100       PERFORM S01-SEND-MAIL                                              
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 D-NEXT-TRANS SECTION.                                                    
038600                                                                          
038700     IF KOMA-KOM-KDKOMSTA = 'S'                                           
038800       PERFORM IMS-DLET-KOMA                                              
038900       PERFORM IMS-GET-KOMA-TRANS                                         
039000       IF SEGMENT-FINNS                                                   
039100         PERFORM S02-ISRT-ALTMSG                                          
039200         IF MSG-KOM-IDMFSMED NOT = '101'                                  
039300           PERFORM IMS-GET-KOMA-ROT                                       
039400           MOVE 'F' TO KOMA-KOM-KDKOMSTA                                  
039500           MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                     
039600           PERFORM IMS-REPL-KOMA                                          
039700         END-IF                                                           
039800       ELSE                                                               
039900         PERFORM IMS-GET-KOMA-ROT                                         
040000         MOVE 'A' TO KOMA-KOM-KDKOMSTA                                    
040100         MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                       
040200         PERFORM IMS-REPL-KOMA                                            
040300       END-IF                                                             
040400     ELSE                                                                 
040500       MOVE '079' TO MED-IDMFSINF                                         
040600       PERFORM S01-SEND-MAIL                                              
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 E-SAMMA-TRANS SECTION.                                                   
041100                                                                          
041200     IF KOMA-KOM-KDKOMSTA = 'S'                                           
041300       PERFORM S02-ISRT-ALTMSG                                            
041400       IF MSG-KOM-IDMFSMED NOT = '249'                                    
041500         PERFORM IMS-GET-KOMA-ROT                                         
041600         MOVE 'F' TO KOMA-KOM-KDKOMSTA                                    
041700         MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                       
041800         PERFORM IMS-REPL-KOMA                                            
041900       END-IF                                                             
042000     ELSE                                                                 
042100       MOVE '079' TO MED-IDMFSINF                                         
042200       PERFORM S01-SEND-MAIL                                              
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 F-GODK-FEL-TRANS SECTION.                                                
042700                                                                          
042800     PERFORM IMS-GET-KOMA-ROT                                             
042900     MOVE 'A' TO KOMA-KOM-KDKOMSTA                                        
043000     MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                           
043100     PERFORM IMS-REPL-KOMA                                                
043200     .                                                                    
043300     EJECT                                                                
043400 G-FEL-TRANS SECTION.                                                     
043500                                                                          
043600     PERFORM IMS-GET-KOMA-ROT                                             
043700     MOVE 'F' TO KOMA-KOM-KDKOMSTA                                        
043800     MOVE MSG-KOM-IDMFSMED TO KOMA-KOM-IDMFSMED                           
043900                              MED-IDMFSINF                                
044000     MOVE MSG-KOM-KDSVAR   TO KOMA-KOM-KDKOMBEH                           
044100     PERFORM IMS-REPL-KOMA                                                
044200                                                                          
044300     IF KOMA-KOM-KDKOMBEH NOT = 'R'                                       
044400                                                                          
044500       CALL WMEDKONV      USING MED-WMEDAREA                              
044600                                                                          
044700       PERFORM S10-OPEN-DP                                                
044800       EVALUATE KOMA-TRAN-IDTRANS                                         
044900         WHEN '4251'                                                      
045000           PERFORM GA-ORDERHUVUD                                          
045100         WHEN '4252'                                                      
045200           PERFORM GB-ORDERRAD                                            
045300         WHEN '4254'                                                      
045400           PERFORM GC-ORDERDELETE                                         
045500         WHEN '4355'                                                      
045600           PERFORM GD-SOFT                                                
045700         WHEN '4360'                                                      
045800           PERFORM GE-DIRLEV                                              
045900         WHEN '4390'                                                      
046000           PERFORM GF-DDGS                                                
046100         WHEN '4258'                                                      
046200           PERFORM GG-API-ORDUPD                                          
046300         WHEN OTHER                                                       
046400           PERFORM GX-SLASK                                               
046500       END-EVALUATE                                                       
046600                                                                          
046700       PERFORM S03-SEND-MFSINF                                            
046800       PERFORM S04-SEND-IMSID                                             
046900                                                                          
047000       PERFORM S10-CLOSE-DP                                               
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 GA-ORDERHUVUD  SECTION.                                                  
047500                                                                          
047600     MOVE SPACE TO WS-IDOUTREC                                            
047700     IF KOMA-KOM-IDSNDNOD(1:4) = 'SOFT'                                   
047800       STRING 'SOFTORDER' 4251-MID-IDDISTR                                
047900       DELIMITED BY SIZE INTO WS-IDOUTREC                                 
048000     ELSE                                                                 
048100      IF KOMA-KOM-IDSNDNOD(1:2) = 'VR' AND                                
048200         KOMA-KOM-IDMFSMED = '703'                                        
048300        STRING 'VR' 4251-MID-IDDISTR                                      
048400        DELIMITED BY SIZE INTO WS-IDOUTREC                                
048500      ELSE                                                                
048600        STRING 'DIST' 4251-MID-IDDISTR                                    
048700        DELIMITED BY SIZE INTO WS-IDOUTREC                                
048800      END-IF                                                              
048900     END-IF                                                               
049000     PERFORM S10-PUT-HDR                                                  
049100                                                                          
049200     MOVE SPACE TO SEND-RAD                                               
049300     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
049400     STRING 'Order Entry Error - order head'                              
049500     DELIMITED BY SIZE INTO MAIL-RAD                                      
049600     PERFORM S10-PUT-LINE                                                 
049700                                                                          
049800     MOVE SPACE TO SEND-RAD                                               
049900     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
050000     STRING 'District: ' 4251-MID-IDDISTR                                 
050100            '  Customer: ' 4251-MID-IDKUNDNR                              
050200     DELIMITED BY SIZE INTO MAIL-RAD                                      
050300     PERFORM S10-PUT-LINE                                                 
050400                                                                          
050500     MOVE SPACE TO SEND-RAD                                               
050600     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
050700     STRING 'Order: ' 4251-MID-IDORDNR                                    
050800            '  Order Class: ' 4251-MID-KDORDKL                            
050900            '  Freight Code: ' 4251-MID-KDFRAKT                           
051000     DELIMITED BY SIZE INTO MAIL-RAD                                      
051100     PERFORM S10-PUT-LINE                                                 
051200                                                                          
051300     MOVE SPACE TO SEND-RAD                                               
051400     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
051500     STRING 'Repair Date   : ' 4251-MID-TIREPDAT                          
051600     DELIMITED BY SIZE INTO MAIL-RAD                                      
051700     PERFORM S10-PUT-LINE                                                 
051800                                                                          
051900     MOVE SPACE TO SEND-RAD                                               
052000     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
052100     STRING 'WIP Number    : ' 4251-MID-BEVARREF                          
052200     DELIMITED BY SIZE INTO MAIL-RAD                                      
052300     PERFORM S10-PUT-LINE                                                 
052400                                                                          
052500     MOVE SPACE TO SEND-RAD                                               
052600     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
052700     STRING 'Iduser        : ' KOMA-KOM-IDUSER                            
052800     DELIMITED BY SIZE INTO MAIL-RAD                                      
052900     PERFORM S10-PUT-LINE                                                 
053000                                                                          
053100     MOVE SPACE TO SEND-RAD                                               
053200     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
053300     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
053400     DELIMITED BY SIZE INTO MAIL-RAD                                      
053500     PERFORM S10-PUT-LINE                                                 
053600                                                                          
053700     MOVE SPACE TO SEND-RAD                                               
053800     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
053900     STRING 'Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                         
054000     DELIMITED BY SIZE INTO MAIL-RAD                                      
054100     PERFORM S10-PUT-LINE                                                 
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500 GB-ORDERRAD SECTION.                                                     
054600                                                                          
054700     MOVE SPACE TO WS-IDOUTREC                                            
054800     STRING 'DIST' 4252-MID-IDDISTR                                       
054900     DELIMITED BY SIZE INTO WS-IDOUTREC                                   
055000     PERFORM S10-PUT-HDR                                                  
055100                                                                          
055200     MOVE SPACE TO SEND-RAD                                               
055300     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
055400     STRING 'Order Entry Error - order lines'                             
055500     DELIMITED BY SIZE INTO MAIL-RAD                                      
055600     PERFORM S10-PUT-LINE                                                 
055700                                                                          
055800     MOVE SPACE TO SEND-RAD                                               
055900     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
056000     STRING 'District: ' 4252-MID-IDDISTR                                 
056100            '  Customer: ' 4252-MID-IDKUNDNR                              
056200     DELIMITED BY SIZE INTO MAIL-RAD                                      
056300     PERFORM S10-PUT-LINE                                                 
056400                                                                          
056500     MOVE SPACE TO SEND-RAD                                               
056600     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
056700     STRING 'Order no: ' 4252-MID-IDORDNR                                 
056800     DELIMITED BY SIZE INTO MAIL-RAD                                      
056900     PERFORM S10-PUT-LINE                                                 
057000                                                                          
057100     MOVE SPACE TO SEND-RAD                                               
057200     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
057300     STRING 'Iduser        : ' KOMA-KOM-IDUSER                            
057400     DELIMITED BY SIZE INTO MAIL-RAD                                      
057500     PERFORM S10-PUT-LINE                                                 
057600                                                                          
057700     MOVE SPACE TO SEND-RAD                                               
057800     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
057900     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
058000     DELIMITED BY SIZE INTO MAIL-RAD                                      
058100     PERFORM S10-PUT-LINE                                                 
058200                                                                          
058300     MOVE SPACE TO SEND-RAD                                               
058400     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
058500     STRING 'Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                         
058600     DELIMITED BY SIZE INTO MAIL-RAD                                      
058700     PERFORM S10-PUT-LINE                                                 
058800     .                                                                    
058900     EJECT                                                                
059000 GC-ORDERDELETE SECTION.                                                  
059100                                                                          
059200     MOVE SPACE          TO WS-IDOUTREC                                   
059300     IF KOMA-KOM-IDSNDNOD(5:4) = 'DLET'                                   
059400       STRING KOMA-KOM-IDSNDNOD (1:4) 4254-MID-IDDISTR                    
059500* OBS! Rule to be created in D&P for new 'Idoutrec' combination           
059600       DELIMITED BY SIZE INTO WS-IDOUTREC                                 
059700     ELSE                                                                 
059800       STRING 'DIST' 4254-MID-IDDISTR                                     
059900       DELIMITED BY SIZE INTO WS-IDOUTREC                                 
060000     END-IF                                                               
060100     PERFORM S10-PUT-HDR                                                  
060200                                                                          
060300     MOVE SPACE          TO SEND-RAD                                      
060400     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
060500     STRING 'Order Delete Error from ' KOMA-KOM-IDSNDNOD (1:4)            
060600     DELIMITED BY SIZE INTO MAIL-RAD                                      
060700     PERFORM S10-PUT-LINE                                                 
060800                                                                          
060900     MOVE SPACE TO SEND-RAD                                               
061000     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
061100     STRING 'District: ' 4254-MID-IDDISTR                                 
061200          '  Customer: ' 4254-MID-IDKUNDNR                                
061300     DELIMITED BY SIZE INTO MAIL-RAD                                      
061400     PERFORM S10-PUT-LINE                                                 
061500                                                                          
061600     MOVE SPACE TO SEND-RAD                                               
061700     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
061800     STRING 'Order: ' 4254-MID-IDORDNR                                    
061900     DELIMITED BY SIZE INTO MAIL-RAD                                      
062000     PERFORM S10-PUT-LINE                                                 
062100                                                                          
062200     MOVE SPACE TO SEND-RAD                                               
062300     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
062400     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
062500     DELIMITED BY SIZE INTO MAIL-RAD                                      
062600     PERFORM S10-PUT-LINE                                                 
062700                                                                          
062800     MOVE SPACE TO SEND-RAD                                               
062900     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
063000     STRING 'Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                         
063100     DELIMITED BY SIZE INTO MAIL-RAD                                      
063200     PERFORM S10-PUT-LINE                                                 
063300     .                                                                    
063400     EJECT                                                                
063500                                                                          
063600 GD-SOFT SECTION.                                                         
063700                                                                          
063800     MOVE SPACE TO WS-IDOUTREC                                            
063900     STRING 'SOFTCANCEL' 4355-MID-IDDISTR                                 
064000     DELIMITED BY SIZE INTO WS-IDOUTREC                                   
064100     PERFORM S10-PUT-HDR                                                  
064200                                                                          
064300     MOVE SPACE TO SEND-RAD                                               
064400     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
064500     STRING 'Error cancelation software, type: '                          
064600                           4355-MID-IDPTYP                                
064700     DELIMITED BY SIZE INTO MAIL-RAD                                      
064800     PERFORM S10-PUT-LINE                                                 
064900                                                                          
065000     MOVE SPACE TO SEND-RAD                                               
065100     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
065200     STRING 'District: ' 4355-MID-IDDISTR                                 
065300            '  Customer: ' 4355-MID-IDKUNDNR                              
065400     DELIMITED BY SIZE INTO MAIL-RAD                                      
065500     PERFORM S10-PUT-LINE                                                 
065600                                                                          
065700     MOVE SPACE TO SEND-RAD                                               
065800     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
065900     STRING 'Order no: ' 4355-MID-IDORDNR7                                
066000            '  Prod no: ' 4355-MID-IDPRODNR                               
066100     DELIMITED BY SIZE INTO MAIL-RAD                                      
066200     PERFORM S10-PUT-LINE                                                 
066300                                                                          
066400     MOVE SPACE TO SEND-RAD                                               
066500     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
066600     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
066700            '   Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                      
066800     DELIMITED BY SIZE INTO MAIL-RAD                                      
066900     PERFORM S10-PUT-LINE                                                 
067000     .                                                                    
067100     EJECT                                                                
067200 GE-DIRLEV SECTION.                                                       
067300                                                                          
067400     MOVE SPACE TO WS-IDOUTREC                                            
067500     STRING 'DLEV' 4360-MID-IDLEVNR                                       
067600     DELIMITED BY SIZE INTO WS-IDOUTREC                                   
067700     PERFORM S10-PUT-HDR                                                  
067800                                                                          
067900     MOVE SPACE TO SEND-RAD                                               
068000     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
068100     STRING 'Order deviations direct delivery: '                          
068200            4360-MID-IDLEVNR                                              
068300     DELIMITED BY SIZE INTO MAIL-RAD                                      
068400     PERFORM S10-PUT-LINE                                                 
068500                                                                          
068600     MOVE SPACE TO SEND-RAD                                               
068700     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
068800     STRING '  Conf. date: ' 4360-MID-DABEKDAT                            
068900            '  Time: ' 4360-MID-TIBEKR                                    
069000     DELIMITED BY SIZE INTO MAIL-RAD                                      
069100     PERFORM S10-PUT-LINE                                                 
069200                                                                          
069300     MOVE SPACE TO SEND-RAD                                               
069400     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
069500     STRING 'District: ' 4360-MID-IDDISTR                                 
069600            '  Customer: ' 4360-MID-IDKUNDNR                              
069700     DELIMITED BY SIZE INTO MAIL-RAD                                      
069800     PERFORM S10-PUT-LINE                                                 
069900                                                                          
070000     MOVE SPACE TO SEND-RAD                                               
070100     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
070200     STRING 'Order no: ' 4360-MID-IDORDNR7                                
070300            '  Prod no: ' 4360-MID-IDPRODNR                               
070400     DELIMITED BY SIZE INTO MAIL-RAD                                      
070500     PERFORM S10-PUT-LINE                                                 
070600                                                                          
070700     MOVE SPACE TO SEND-RAD                                               
070800     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
070900     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
071000            '   Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                      
071100     DELIMITED BY SIZE INTO MAIL-RAD                                      
071200     PERFORM S10-PUT-LINE                                                 
071300     .                                                                    
071400     EJECT                                                                
071500 GF-DDGS SECTION.                                                         
071600                                                                          
071700     MOVE SPACE TO WS-IDOUTREC                                            
071800     STRING 'DDGS' 4390-MID-IDANSTNR                                      
071900     DELIMITED BY SIZE INTO WS-IDOUTREC                                   
072000     PERFORM S10-PUT-HDR                                                  
072100                                                                          
072200     MOVE SPACE TO SEND-RAD                                               
072300     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
072400     STRING 'Error packing DDGS. Supplier: '                              
072500            4390-MID-IDANSTNR ' Dc: ' 4390-MID-IDDC                       
072600     DELIMITED BY SIZE INTO MAIL-RAD                                      
072700     PERFORM S10-PUT-LINE                                                 
072800                                                                          
072900     MOVE SPACE TO SEND-RAD                                               
073000     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
073100     STRING 'District: ' 4390-MID-IDDISTR                                 
073200            '  Customer: ' 4390-MID-IDKUNDNR                              
073300     DELIMITED BY SIZE INTO MAIL-RAD                                      
073400     PERFORM S10-PUT-LINE                                                 
073500                                                                          
073600     MOVE SPACE TO SEND-RAD                                               
073700     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
073800     STRING 'Order no: ' 4390-MID-IDORDNR                                 
073900            '  Prod no: ' 4390-MID-IDPRODNR                               
074000     DELIMITED BY SIZE INTO MAIL-RAD                                      
074100     PERFORM S10-PUT-LINE                                                 
074200                                                                          
074300     MOVE SPACE TO SEND-RAD                                               
074400     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
074500     STRING 'Case: ' 4390-MID-IDKOLLI                                     
074600            '  Sending date: ' 4390-MID-DASUPREF                          
074700     DELIMITED BY SIZE INTO MAIL-RAD                                      
074800     PERFORM S10-PUT-LINE                                                 
074900                                                                          
075000     MOVE SPACE TO SEND-RAD                                               
075100     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
075200     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
075300            '   Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                      
075400     DELIMITED BY SIZE INTO MAIL-RAD                                      
075500     PERFORM S10-PUT-LINE                                                 
075600     .                                                                    
075700     EJECT                                                                
075800 GG-API-ORDUPD SECTION.                                                   
075900                                                                          
076000     MOVE SPACE TO WS-IDOUTREC                                            
076100     IF KOMA-KOM-IDSNDNOD(1:4) = 'API '                                   
076200       STRING 'API' 4258-MID-IDDISTR                                      
076300       DELIMITED BY SIZE INTO WS-IDOUTREC                                 
076400     END-IF                                                               
076500     PERFORM S10-PUT-HDR                                                  
076600                                                                          
076700     MOVE SPACE TO SEND-RAD                                               
076800     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
076900     STRING 'API Order Update Error'                                      
077000     DELIMITED BY SIZE INTO MAIL-RAD                                      
077100     PERFORM S10-PUT-LINE                                                 
077200                                                                          
077300     MOVE SPACE TO SEND-RAD                                               
077400     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
077500     STRING 'District: ' 4258-MID-IDDISTR                                 
077600            '  Customer: ' 4258-MID-IDKUNDNR                              
077700            '  Order no: ' 4258-MID-IDORDNR7                              
077800     DELIMITED BY SIZE INTO MAIL-RAD                                      
077900     PERFORM S10-PUT-LINE                                                 
078000                                                                          
078100     MOVE SPACE TO SEND-RAD                                               
078200     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
078300     STRING 'Order from: ' 4258-MID-IDSYSTEM                              
078400            '  Reg. Date : ' 4258-MID-TIREGDAT                            
078500     DELIMITED BY SIZE INTO MAIL-RAD                                      
078600     PERFORM S10-PUT-LINE                                                 
078700                                                                          
078800     MOVE SPACE TO SEND-RAD                                               
078900     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
079000     STRING 'Iduser        : ' KOMA-KOM-IDUSER                            
079100     DELIMITED BY SIZE INTO MAIL-RAD                                      
079200     PERFORM S10-PUT-LINE                                                 
079300                                                                          
079400     MOVE SPACE TO SEND-RAD                                               
079500     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
079600     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
079700     DELIMITED BY SIZE INTO MAIL-RAD                                      
079800     PERFORM S10-PUT-LINE                                                 
079900                                                                          
080000     MOVE SPACE TO SEND-RAD                                               
080100     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
080200     STRING 'Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                         
080300     DELIMITED BY SIZE INTO MAIL-RAD                                      
080400     PERFORM S10-PUT-LINE                                                 
080500     .                                                                    
080600     EJECT                                                                
080700                                                                          
080800 GX-SLASK SECTION.                                                        
080900                                                                          
081000     MOVE SPACE TO WS-IDOUTREC                                            
081100     MOVE 'SLASK' TO WS-IDOUTREC                                          
081200     PERFORM S10-PUT-HDR                                                  
081300                                                                          
081400     MOVE SPACE TO SEND-RAD                                               
081500     MOVE WS-SKIP3 TO STYRTECKEN-RAD                                      
081600     STRING 'Error in dispatch. For below data use cpy: '                 
081700                               MSG-KOM-IDCPYTXT                           
081800     DELIMITED BY SIZE INTO MAIL-RAD                                      
081900     PERFORM S10-PUT-LINE                                                 
082000                                                                          
082100     MOVE SPACE TO SEND-RAD                                               
082200     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
082300     MOVE KOMA-TRAN-INDATA (1:80) TO MAIL-RAD                             
082400     PERFORM S10-PUT-LINE                                                 
082500                                                                          
082600     MOVE SPACE TO SEND-RAD                                               
082700     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
082800     STRING 'Dispatch Reg.Date: ' MSG-KOM-TIREGDAT                        
082900            '   Dispatch Reg.Time: ' MSG-KOM-TIKLOCK                      
083000     DELIMITED BY SIZE INTO MAIL-RAD                                      
083100     PERFORM S10-PUT-LINE                                                 
083200                                                                          
083300     MOVE SPACE TO SEND-RAD                                               
083400     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
083500     STRING 'Iduser        : ' KOMA-KOM-IDUSER                            
083600     DELIMITED BY SIZE INTO MAIL-RAD                                      
083700     PERFORM S10-PUT-LINE                                                 
083800     .                                                                    
083900     EJECT                                                                
084000 S01-SEND-MAIL SECTION.                                                   
084100                                                                          
084200     PERFORM S10-OPEN-DP                                                  
084300                                                                          
084400     MOVE 'BUGG' TO WS-IDOUTREC                                           
084500     PERFORM S10-PUT-HDR                                                  
084600                                                                          
084700     CALL WMEDKONV  USING MED-WMEDAREA                                    
084800     PERFORM S03-SEND-MFSINF                                              
084900                                                                          
085000     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
085100     MOVE SPACE TO SEND-RAD                                               
085200     STRING 'Something went wrong!? Possible wrong restart? '             
085300                     KOMA-TRAN-INDATA (1:80)                              
085400     DELIMITED BY SIZE INTO MAIL-RAD                                      
085500     PERFORM S10-PUT-LINE                                                 
085600                                                                          
085700     MOVE SPACE TO SEND-RAD                                               
085800     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
085900     STRING 'Node : ' MSG-KOM-IDSNDNOD                                    
086000            '    Job: ' MSG-KOM-IDSNDJOB                                  
086100     DELIMITED BY SIZE INTO MAIL-RAD                                      
086200     PERFORM S10-PUT-LINE                                                 
086300                                                                          
086400     MOVE SPACE TO SEND-RAD                                               
086500     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
086600     STRING 'Date: ' MSG-KOM-TIREGDAT                                     
086700            '      Time: '  MSG-KOM-TIKLOCK                               
086800     DELIMITED BY SIZE INTO MAIL-RAD                                      
086900     PERFORM S10-PUT-LINE                                                 
087000                                                                          
087100     PERFORM S04-SEND-IMSID                                               
087200                                                                          
087300     IF MED-IDMFSINF = '078'                                              
087400       MOVE SPACE TO SEND-RAD                                             
087500       MOVE WS-SKIP1 TO STYRTECKEN-RAD                                    
087600       STRING 'Status: ' STATUS-WS                                        
087700       DELIMITED BY SIZE INTO MAIL-RAD                                    
087800       PERFORM S10-PUT-LINE                                               
087900     END-IF                                                               
088000                                                                          
088100     PERFORM S10-CLOSE-DP                                                 
088200*    MOVE MSG-KOM-IDCPYTXT TO W-IDCPYTXT-DATA                             
088300*    MOVE KOMA-KOM-IDLTERM TO W-IDLTERM-DATA                              
088400*    MOVE KOMA-KOM-IDUSER TO W-IDUSER-DATA                                
088500*    MOVE MSG-KOM-IDMFSMED TO W-IDMFSMED-DATA                             
088600*    MOVE KOMA-KOM-KDKOMSTA TO W-KDKOMSTA-DATA                            
088700*    MOVE KOMA-KOM-KDKOMBEH TO W-KDKOMBEH-DATA                            
088800*    MOVE KOMA-TRAN-IDUSER TO W-IDUSER-DATA2                              
088900     .                                                                    
089000     EJECT                                                                
089100 S02-ISRT-ALTMSG SECTION.                                                 
089200                                                                          
089300     MOVE KOMA-TRAN-WMSGAREA TO MSG-IO-AREA                               
089400     EVALUATE MSG-KDTRANS-1                                               
089500       WHEN 'W1T112X ' PERFORM IMS-ISRT-1112                              
089600       WHEN 'W1T113X ' PERFORM IMS-ISRT-1113                              
089700       WHEN 'W1T116X ' PERFORM IMS-ISRT-1116                              
089800       WHEN 'W1T117X ' PERFORM IMS-ISRT-1117                              
089900       WHEN 'W2T109X ' PERFORM IMS-ISRT-2109                              
090000       WHEN 'W2T133X ' PERFORM IMS-ISRT-2133                              
090100       WHEN 'W2T163X ' PERFORM IMS-ISRT-2163                              
090200       WHEN 'W2T191X ' PERFORM IMS-ISRT-2191                              
090300       WHEN 'W2T333X ' PERFORM IMS-ISRT-2333                              
090400       WHEN 'W2T335X ' PERFORM IMS-ISRT-2335                              
090500       WHEN 'W2T337X ' PERFORM IMS-ISRT-2337                              
090600       WHEN 'W2T403X ' PERFORM IMS-ISRT-2403                              
090700       WHEN 'W3T151X ' PERFORM IMS-ISRT-3151                              
090800       WHEN 'W4T251X ' PERFORM IMS-ISRT-4251                              
090900       WHEN 'W4T252X ' PERFORM IMS-ISRT-4252                              
091000       WHEN 'W4T252Y ' PERFORM IMS-ISRT-4252Y                             
091100       WHEN 'W4T253X ' PERFORM IMS-ISRT-4253                              
091200       WHEN 'W4T254X ' PERFORM IMS-ISRT-4254                              
091300       WHEN 'W4T255X ' PERFORM IMS-ISRT-4255                              
091400       WHEN 'W4T256X ' PERFORM IMS-ISRT-4256                              
091500       WHEN 'W4T258X ' PERFORM IMS-ISRT-4258                              
091600       WHEN 'W4T261X ' PERFORM IMS-ISRT-4261                              
091700       WHEN 'W4T262X ' PERFORM IMS-ISRT-4262                              
091800       WHEN 'W4T263X ' PERFORM IMS-ISRT-4263                              
091900       WHEN 'W4T355X ' PERFORM IMS-ISRT-4355                              
092000       WHEN 'W4T360X ' PERFORM IMS-ISRT-4360                              
092100       WHEN 'W4T390X ' PERFORM IMS-ISRT-4390                              
092200       WHEN 'W4T399X ' PERFORM IMS-ISRT-4399                              
092300       WHEN 'W4T723U ' PERFORM IMS-ISRT-4723                              
092400       WHEN 'W4T791X ' PERFORM IMS-ISRT-4791                              
092500       WHEN 'W4T792X ' PERFORM IMS-ISRT-4792                              
092600       WHEN 'W4T797X ' PERFORM IMS-ISRT-4797                              
092700       WHEN 'W5T111X ' PERFORM IMS-ISRT-5111                              
092800       WHEN 'W5T112X ' PERFORM IMS-ISRT-5112                              
092900       WHEN 'W5T119X ' PERFORM IMS-ISRT-5119                              
093000       WHEN 'W5T206X ' PERFORM IMS-ISRT-5206                              
093100       WHEN 'W6T11BX ' PERFORM IMS-ISRT-611B                              
093200       WHEN 'W6T11CX ' PERFORM IMS-ISRT-611C                              
093300       WHEN 'W6T11DX ' PERFORM IMS-ISRT-611D                              
093400       WHEN 'W6T192X ' PERFORM IMS-ISRT-6192                              
093500       WHEN 'W6T193X ' PERFORM IMS-ISRT-6193                              
093600       WHEN 'W6T19BX ' PERFORM IMS-ISRT-619B                              
093700       WHEN 'WZ0430X ' PERFORM IMS-ISRT-Z430                              
093800       WHEN OTHER                                                         
093900                       PERFORM IMS-CHANGE-ALTMSG                          
094000                       IF STATUS-OK                                       
094100                         PERFORM IMS-ISRT-ALTMSG                          
094200                       ELSE                                               
094300                         MOVE '078' TO MED-IDMFSINF                       
094400                         PERFORM S01-SEND-MAIL                            
094500                       END-IF                                             
094600     END-EVALUATE                                                         
094700     .                                                                    
094800     EJECT                                                                
094900 S03-SEND-MFSINF SECTION.                                                 
095000                                                                          
095100     MOVE SPACE TO SEND-RAD                                               
095200     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
095300     STRING 'Error Message : ' MED-IDMFSINF                               
095400            ' - ' MED-TEMFSINF                                            
095500     DELIMITED BY SIZE INTO MAIL-RAD                                      
095600                                                                          
095700     PERFORM S10-PUT-LINE                                                 
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100 S04-SEND-IMSID SECTION.                                                  
096200                                                                          
096300     MOVE SPACE TO SEND-RAD                                               
096400     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
096500     STRING 'Environment : ' W-IMSID                                      
096600     DELIMITED BY SIZE INTO MAIL-RAD                                      
096700                                                                          
096800     PERFORM S10-PUT-LINE                                                 
096900     .                                                                    
097000     EJECT                                                                
097100 S10-OPEN-DP SECTION.                                                     
097200                                                                          
097300     MOVE 'OPEN'                        TO SEND-KDFUNC                    
097400     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
097500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
097600                         SEND-OPEN-AREA                                   
097700     IF SEND-KDRC > 0                                                     
097800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
097900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
098000       DELIMITED BY SIZE INTO FELTEXT                                     
098100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500 S10-PUT-HDR SECTION.                                                     
098600                                                                          
098700     MOVE 1                       TO REQU-IDMSGVER                        
098800     MOVE ' '                     TO REQU-KDPGMACT                        
098900     MOVE IDPGM                   TO REQU-IDUSER                          
099000     MOVE 'DISP-ERROR'            TO HDR-IDOUTTYPE                        
099100     MOVE WS-IDOUTREC             TO HDR-IDOUTREC                         
099200     MOVE SPACE                   TO HDR-IDLIST                           
099300     MOVE 'PUT'                   TO SEND-KDFUNC                          
099400     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
099500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
099600                         SEND-KVDLEN                                      
099700                         HDR-AREA                                         
099800     IF SEND-KDRC > ZERO                                                  
099900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
100000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
100100       DELIMITED BY SIZE INTO FELTEXT                                     
100200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
100300     END-IF                                                               
100400     .                                                                    
100500     EJECT                                                                
100600 S10-PUT-LINE SECTION.                                                    
100700                                                                          
100800     MOVE 'PUT'                           TO SEND-KDFUNC                  
100900     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
101000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
101100                         SEND-KVDLEN                                      
101200                         SEND-RAD                                         
101300     IF SEND-KDRC > ZERO                                                  
101400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
101500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
101600       DELIMITED BY SIZE INTO FELTEXT                                     
101700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100 S10-CLOSE-DP SECTION.                                                    
102200                                                                          
102300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
102400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
102500                                                                          
102600     IF SEND-KDRC > 0                                                     
102700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
102800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
102900       DELIMITED BY SIZE INTO FELTEXT                                     
103000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
103100     END-IF                                                               
103200     .                                                                    
103300     EJECT                                                                
103400* IMS SEKTIONER                                                           
103500                                                                          
103600 IMS-GET-MSG SECTION.                                                     
103700                                                                          
103800     MOVE '  QC' TO GODK-STATUSKODER                                      
103900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
104000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300 IMS-CHANGE-ALTMSG SECTION.                                               
104400                                                                          
104500     MOVE '  A1A4' TO GODK-STATUSKODER                                    
104600     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
104700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
104800     PERFORM IMS-STATUSKONTROLL                                           
104900     .                                                                    
105000 IMS-ISRT-ALTMSG SECTION.                                                 
105100                                                                          
105200     MOVE SPACE TO GODK-STATUSKODER                                       
105300     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
105400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     CALL CBLTDLI USING ISRT ALT-PCB MSG-KOM-WMSGKOM                      
105700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
105800     PERFORM IMS-STATUSKONTROLL                                           
105900     .                                                                    
106000 IMS-ISRT-1112 SECTION.                                                   
106100                                                                          
106200     MOVE SPACE TO GODK-STATUSKODER                                       
106300     CALL CBLTDLI USING ISRT ALT1112-PCB MSG-IO-AREA                      
106400     MOVE ALT1112-STATUS-CODE TO STATUS-WS                                
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     CALL CBLTDLI USING ISRT ALT1112-PCB MSG-KOM-WMSGKOM                  
106700     MOVE ALT1112-STATUS-CODE TO STATUS-WS                                
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     .                                                                    
107000 IMS-ISRT-1113 SECTION.                                                   
107100                                                                          
107200     MOVE SPACE TO GODK-STATUSKODER                                       
107300     CALL CBLTDLI USING ISRT ALT1113-PCB MSG-IO-AREA                      
107400     MOVE ALT1113-STATUS-CODE TO STATUS-WS                                
107500     PERFORM IMS-STATUSKONTROLL                                           
107600     CALL CBLTDLI USING ISRT ALT1113-PCB MSG-KOM-WMSGKOM                  
107700     MOVE ALT1113-STATUS-CODE TO STATUS-WS                                
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000 IMS-ISRT-1116 SECTION.                                                   
108100                                                                          
108200     MOVE SPACE TO GODK-STATUSKODER                                       
108300     CALL CBLTDLI USING ISRT ALT1116-PCB MSG-IO-AREA                      
108400     MOVE ALT1116-STATUS-CODE TO STATUS-WS                                
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     CALL CBLTDLI USING ISRT ALT1116-PCB MSG-KOM-WMSGKOM                  
108700     MOVE ALT1116-STATUS-CODE TO STATUS-WS                                
108800     PERFORM IMS-STATUSKONTROLL                                           
108900     .                                                                    
109000 IMS-ISRT-1117 SECTION.                                                   
109100                                                                          
109200     MOVE SPACE TO GODK-STATUSKODER                                       
109300     CALL CBLTDLI USING ISRT ALT1117-PCB MSG-IO-AREA                      
109400     MOVE ALT1117-STATUS-CODE TO STATUS-WS                                
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     CALL CBLTDLI USING ISRT ALT1117-PCB MSG-KOM-WMSGKOM                  
109700     MOVE ALT1117-STATUS-CODE TO STATUS-WS                                
109800     PERFORM IMS-STATUSKONTROLL                                           
109900     .                                                                    
110000 IMS-ISRT-2109 SECTION.                                                   
110100                                                                          
110200     MOVE SPACE TO GODK-STATUSKODER                                       
110300     CALL CBLTDLI USING ISRT ALT2109-PCB MSG-IO-AREA                      
110400     MOVE ALT2109-STATUS-CODE TO STATUS-WS                                
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     CALL CBLTDLI USING ISRT ALT2109-PCB MSG-KOM-WMSGKOM                  
110700     MOVE ALT2109-STATUS-CODE TO STATUS-WS                                
110800     PERFORM IMS-STATUSKONTROLL                                           
110900     .                                                                    
111000 IMS-ISRT-2133 SECTION.                                                   
111100                                                                          
111200     MOVE SPACE TO GODK-STATUSKODER                                       
111300     CALL CBLTDLI USING ISRT ALT2133-PCB MSG-IO-AREA                      
111400     MOVE ALT2133-STATUS-CODE TO STATUS-WS                                
111500     PERFORM IMS-STATUSKONTROLL                                           
111600     CALL CBLTDLI USING ISRT ALT2133-PCB MSG-KOM-WMSGKOM                  
111700     MOVE ALT2133-STATUS-CODE TO STATUS-WS                                
111800     PERFORM IMS-STATUSKONTROLL                                           
111900     .                                                                    
112000 IMS-ISRT-2163 SECTION.                                                   
112100                                                                          
112200     MOVE SPACE TO GODK-STATUSKODER                                       
112300     CALL CBLTDLI USING ISRT ALT2163-PCB MSG-IO-AREA                      
112400     MOVE ALT2163-STATUS-CODE TO STATUS-WS                                
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     CALL CBLTDLI USING ISRT ALT2163-PCB MSG-KOM-WMSGKOM                  
112700     MOVE ALT2163-STATUS-CODE TO STATUS-WS                                
112800     PERFORM IMS-STATUSKONTROLL                                           
112900     .                                                                    
113000 IMS-ISRT-2191 SECTION.                                                   
113100                                                                          
113200     MOVE SPACE TO GODK-STATUSKODER                                       
113300     CALL CBLTDLI USING ISRT ALT2191-PCB MSG-IO-AREA                      
113400     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
113500     PERFORM IMS-STATUSKONTROLL                                           
113600     CALL CBLTDLI USING ISRT ALT2191-PCB MSG-KOM-WMSGKOM                  
113700     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     .                                                                    
114000 IMS-ISRT-2333 SECTION.                                                   
114100                                                                          
114200     MOVE SPACE TO GODK-STATUSKODER                                       
114300     CALL CBLTDLI USING ISRT ALT2333-PCB MSG-IO-AREA                      
114400     MOVE ALT2333-STATUS-CODE TO STATUS-WS                                
114500     PERFORM IMS-STATUSKONTROLL                                           
114600     CALL CBLTDLI USING ISRT ALT2333-PCB MSG-KOM-WMSGKOM                  
114700     MOVE ALT2333-STATUS-CODE TO STATUS-WS                                
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000 IMS-ISRT-2335 SECTION.                                                   
115100                                                                          
115200     MOVE SPACE TO GODK-STATUSKODER                                       
115300     CALL CBLTDLI USING ISRT ALT2335-PCB MSG-IO-AREA                      
115400     MOVE ALT2335-STATUS-CODE TO STATUS-WS                                
115500     PERFORM IMS-STATUSKONTROLL                                           
115600     CALL CBLTDLI USING ISRT ALT2335-PCB MSG-KOM-WMSGKOM                  
115700     MOVE ALT2335-STATUS-CODE TO STATUS-WS                                
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000 IMS-ISRT-2337 SECTION.                                                   
116100                                                                          
116200     MOVE SPACE TO GODK-STATUSKODER                                       
116300     CALL CBLTDLI USING ISRT ALT2337-PCB MSG-IO-AREA                      
116400     MOVE ALT2337-STATUS-CODE TO STATUS-WS                                
116500     PERFORM IMS-STATUSKONTROLL                                           
116600     CALL CBLTDLI USING ISRT ALT2337-PCB MSG-KOM-WMSGKOM                  
116700     MOVE ALT2337-STATUS-CODE TO STATUS-WS                                
116800     PERFORM IMS-STATUSKONTROLL                                           
116900     .                                                                    
117000 IMS-ISRT-2403 SECTION.                                                   
117100                                                                          
117200     MOVE SPACE TO GODK-STATUSKODER                                       
117300     CALL CBLTDLI USING ISRT ALT2403-PCB MSG-IO-AREA                      
117400     MOVE ALT2403-STATUS-CODE TO STATUS-WS                                
117500     PERFORM IMS-STATUSKONTROLL                                           
117600     CALL CBLTDLI USING ISRT ALT2403-PCB MSG-KOM-WMSGKOM                  
117700     MOVE ALT2403-STATUS-CODE TO STATUS-WS                                
117800     PERFORM IMS-STATUSKONTROLL                                           
117900     .                                                                    
118000 IMS-ISRT-3151 SECTION.                                                   
118100                                                                          
118200     MOVE SPACE TO GODK-STATUSKODER                                       
118300     CALL CBLTDLI USING ISRT ALT3151-PCB MSG-IO-AREA                      
118400     MOVE ALT3151-STATUS-CODE TO STATUS-WS                                
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     CALL CBLTDLI USING ISRT ALT3151-PCB MSG-KOM-WMSGKOM                  
118700     MOVE ALT3151-STATUS-CODE TO STATUS-WS                                
118800     PERFORM IMS-STATUSKONTROLL                                           
118900     .                                                                    
119000 IMS-ISRT-4251 SECTION.                                                   
119100                                                                          
119200     MOVE SPACE TO GODK-STATUSKODER                                       
119300     CALL CBLTDLI USING ISRT ALT4251-PCB MSG-IO-AREA                      
119400     MOVE ALT4251-STATUS-CODE TO STATUS-WS                                
119500     PERFORM IMS-STATUSKONTROLL                                           
119600     CALL CBLTDLI USING ISRT ALT4251-PCB MSG-KOM-WMSGKOM                  
119700     MOVE ALT4251-STATUS-CODE TO STATUS-WS                                
119800     PERFORM IMS-STATUSKONTROLL                                           
119900     .                                                                    
120000 IMS-ISRT-4252 SECTION.                                                   
120100                                                                          
120200     MOVE SPACE TO GODK-STATUSKODER                                       
120300     CALL CBLTDLI USING ISRT ALT4252-PCB MSG-IO-AREA                      
120400     MOVE ALT4252-STATUS-CODE TO STATUS-WS                                
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     CALL CBLTDLI USING ISRT ALT4252-PCB MSG-KOM-WMSGKOM                  
120700     MOVE ALT4252-STATUS-CODE TO STATUS-WS                                
120800     PERFORM IMS-STATUSKONTROLL                                           
120900     .                                                                    
121000 IMS-ISRT-4252Y SECTION.                                                  
121100                                                                          
121200     MOVE SPACE TO GODK-STATUSKODER                                       
121300     CALL CBLTDLI USING ISRT ALT4252Y-PCB MSG-IO-AREA                     
121400     MOVE ALT4252Y-STATUS-CODE TO STATUS-WS                               
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     CALL CBLTDLI USING ISRT ALT4252Y-PCB MSG-KOM-WMSGKOM                 
121700     MOVE ALT4252Y-STATUS-CODE TO STATUS-WS                               
121800     PERFORM IMS-STATUSKONTROLL                                           
121900     .                                                                    
122000 IMS-ISRT-4253 SECTION.                                                   
122100                                                                          
122200     MOVE SPACE TO GODK-STATUSKODER                                       
122300     CALL CBLTDLI USING ISRT ALT4253-PCB MSG-IO-AREA                      
122400     MOVE ALT4253-STATUS-CODE TO STATUS-WS                                
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     CALL CBLTDLI USING ISRT ALT4253-PCB MSG-KOM-WMSGKOM                  
122700     MOVE ALT4253-STATUS-CODE TO STATUS-WS                                
122800     PERFORM IMS-STATUSKONTROLL                                           
122900     .                                                                    
123000 IMS-ISRT-4254 SECTION.                                                   
123100                                                                          
123200     MOVE SPACE TO GODK-STATUSKODER                                       
123300     CALL CBLTDLI USING ISRT ALT4254-PCB MSG-IO-AREA                      
123400     MOVE ALT4254-STATUS-CODE TO STATUS-WS                                
123500     PERFORM IMS-STATUSKONTROLL                                           
123600     CALL CBLTDLI USING ISRT ALT4254-PCB MSG-KOM-WMSGKOM                  
123700     MOVE ALT4254-STATUS-CODE TO STATUS-WS                                
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000 IMS-ISRT-4255 SECTION.                                                   
124100                                                                          
124200     MOVE SPACE TO GODK-STATUSKODER                                       
124300     CALL CBLTDLI USING ISRT ALT4255-PCB MSG-IO-AREA                      
124400     MOVE ALT4255-STATUS-CODE TO STATUS-WS                                
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     CALL CBLTDLI USING ISRT ALT4255-PCB MSG-KOM-WMSGKOM                  
124700     MOVE ALT4255-STATUS-CODE TO STATUS-WS                                
124800     PERFORM IMS-STATUSKONTROLL                                           
124900     .                                                                    
125000 IMS-ISRT-4256 SECTION.                                                   
125100                                                                          
125200     MOVE SPACE TO GODK-STATUSKODER                                       
125300     CALL CBLTDLI USING ISRT ALT4256-PCB MSG-IO-AREA                      
125400     MOVE ALT4256-STATUS-CODE TO STATUS-WS                                
125500     PERFORM IMS-STATUSKONTROLL                                           
125600     CALL CBLTDLI USING ISRT ALT4256-PCB MSG-KOM-WMSGKOM                  
125700     MOVE ALT4256-STATUS-CODE TO STATUS-WS                                
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     .                                                                    
126000 IMS-ISRT-4258 SECTION.                                                   
126100                                                                          
126200     MOVE SPACE TO GODK-STATUSKODER                                       
126300     CALL CBLTDLI USING ISRT ALT4258-PCB MSG-IO-AREA                      
126400     MOVE ALT4258-STATUS-CODE TO STATUS-WS                                
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     CALL CBLTDLI USING ISRT ALT4258-PCB MSG-KOM-WMSGKOM                  
126700     MOVE ALT4258-STATUS-CODE TO STATUS-WS                                
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000 IMS-ISRT-4261 SECTION.                                                   
127100                                                                          
127200     MOVE SPACE TO GODK-STATUSKODER                                       
127300     CALL CBLTDLI USING ISRT ALT4261-PCB MSG-IO-AREA                      
127400     MOVE ALT4261-STATUS-CODE TO STATUS-WS                                
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     CALL CBLTDLI USING ISRT ALT4261-PCB MSG-KOM-WMSGKOM                  
127700     MOVE ALT4261-STATUS-CODE TO STATUS-WS                                
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000 IMS-ISRT-4262 SECTION.                                                   
128100                                                                          
128200     MOVE SPACE TO GODK-STATUSKODER                                       
128300     CALL CBLTDLI USING ISRT ALT4262-PCB MSG-IO-AREA                      
128400     MOVE ALT4262-STATUS-CODE TO STATUS-WS                                
128500     PERFORM IMS-STATUSKONTROLL                                           
128600     CALL CBLTDLI USING ISRT ALT4262-PCB MSG-KOM-WMSGKOM                  
128700     MOVE ALT4262-STATUS-CODE TO STATUS-WS                                
128800     PERFORM IMS-STATUSKONTROLL                                           
128900     .                                                                    
129000 IMS-ISRT-4263 SECTION.                                                   
129100                                                                          
129200     MOVE SPACE TO GODK-STATUSKODER                                       
129300     CALL CBLTDLI USING ISRT ALT4263-PCB MSG-IO-AREA                      
129400     MOVE ALT4263-STATUS-CODE TO STATUS-WS                                
129500     PERFORM IMS-STATUSKONTROLL                                           
129600     CALL CBLTDLI USING ISRT ALT4263-PCB MSG-KOM-WMSGKOM                  
129700     MOVE ALT4263-STATUS-CODE TO STATUS-WS                                
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000 IMS-ISRT-4355 SECTION.                                                   
130100                                                                          
130200     MOVE SPACE TO GODK-STATUSKODER                                       
130300     CALL CBLTDLI USING ISRT ALT4355-PCB MSG-IO-AREA                      
130400     MOVE ALT4355-STATUS-CODE TO STATUS-WS                                
130500     PERFORM IMS-STATUSKONTROLL                                           
130600     CALL CBLTDLI USING ISRT ALT4355-PCB MSG-KOM-WMSGKOM                  
130700     MOVE ALT4355-STATUS-CODE TO STATUS-WS                                
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000 IMS-ISRT-4360 SECTION.                                                   
131100                                                                          
131200     MOVE SPACE TO GODK-STATUSKODER                                       
131300     CALL CBLTDLI USING ISRT ALT4360-PCB MSG-IO-AREA                      
131400     MOVE ALT4360-STATUS-CODE TO STATUS-WS                                
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     CALL CBLTDLI USING ISRT ALT4360-PCB MSG-KOM-WMSGKOM                  
131700     MOVE ALT4360-STATUS-CODE TO STATUS-WS                                
131800     PERFORM IMS-STATUSKONTROLL                                           
131900     .                                                                    
132000 IMS-ISRT-4390 SECTION.                                                   
132100                                                                          
132200     MOVE SPACE TO GODK-STATUSKODER                                       
132300     CALL CBLTDLI USING ISRT ALT4390-PCB MSG-IO-AREA                      
132400     MOVE ALT4390-STATUS-CODE TO STATUS-WS                                
132500     PERFORM IMS-STATUSKONTROLL                                           
132600     CALL CBLTDLI USING ISRT ALT4390-PCB MSG-KOM-WMSGKOM                  
132700     MOVE ALT4390-STATUS-CODE TO STATUS-WS                                
132800     PERFORM IMS-STATUSKONTROLL                                           
132900     .                                                                    
133000 IMS-ISRT-4399 SECTION.                                                   
133100                                                                          
133200     MOVE SPACE TO GODK-STATUSKODER                                       
133300     CALL CBLTDLI USING ISRT ALT4399-PCB MSG-IO-AREA                      
133400     MOVE ALT4399-STATUS-CODE TO STATUS-WS                                
133500     PERFORM IMS-STATUSKONTROLL                                           
133600     CALL CBLTDLI USING ISRT ALT4399-PCB MSG-KOM-WMSGKOM                  
133700     MOVE ALT4399-STATUS-CODE TO STATUS-WS                                
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000 IMS-ISRT-4723 SECTION.                                                   
134100                                                                          
134200     MOVE SPACE TO GODK-STATUSKODER                                       
134300     CALL CBLTDLI USING ISRT ALT4723-PCB MSG-IO-AREA                      
134400     MOVE ALT4723-STATUS-CODE TO STATUS-WS                                
134500     PERFORM IMS-STATUSKONTROLL                                           
134600     CALL CBLTDLI USING ISRT ALT4723-PCB MSG-KOM-WMSGKOM                  
134700     MOVE ALT4723-STATUS-CODE TO STATUS-WS                                
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000 IMS-ISRT-4791 SECTION.                                                   
135100                                                                          
135200     MOVE SPACE TO GODK-STATUSKODER                                       
135300     CALL CBLTDLI USING ISRT ALT4791-PCB MSG-IO-AREA                      
135400     MOVE ALT4791-STATUS-CODE TO STATUS-WS                                
135500     PERFORM IMS-STATUSKONTROLL                                           
135600     CALL CBLTDLI USING ISRT ALT4791-PCB MSG-KOM-WMSGKOM                  
135700     MOVE ALT4791-STATUS-CODE TO STATUS-WS                                
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000 IMS-ISRT-4792 SECTION.                                                   
136100                                                                          
136200     MOVE SPACE TO GODK-STATUSKODER                                       
136300     CALL CBLTDLI USING ISRT ALT4792-PCB MSG-IO-AREA                      
136400     MOVE ALT4792-STATUS-CODE TO STATUS-WS                                
136500     PERFORM IMS-STATUSKONTROLL                                           
136600     CALL CBLTDLI USING ISRT ALT4792-PCB MSG-KOM-WMSGKOM                  
136700     MOVE ALT4792-STATUS-CODE TO STATUS-WS                                
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000 IMS-ISRT-4797 SECTION.                                                   
137100                                                                          
137200     MOVE SPACE TO GODK-STATUSKODER                                       
137300     CALL CBLTDLI USING ISRT ALT4797-PCB MSG-IO-AREA                      
137400     MOVE ALT4797-STATUS-CODE TO STATUS-WS                                
137500     PERFORM IMS-STATUSKONTROLL                                           
137600     CALL CBLTDLI USING ISRT ALT4797-PCB MSG-KOM-WMSGKOM                  
137700     MOVE ALT4797-STATUS-CODE TO STATUS-WS                                
137800     PERFORM IMS-STATUSKONTROLL                                           
137900     .                                                                    
138000 IMS-ISRT-5111 SECTION.                                                   
138100                                                                          
138200     MOVE SPACE TO GODK-STATUSKODER                                       
138300     CALL CBLTDLI USING ISRT ALT5111-PCB MSG-IO-AREA                      
138400     MOVE ALT5111-STATUS-CODE TO STATUS-WS                                
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     CALL CBLTDLI USING ISRT ALT5111-PCB MSG-KOM-WMSGKOM                  
138700     MOVE ALT5111-STATUS-CODE TO STATUS-WS                                
138800     PERFORM IMS-STATUSKONTROLL                                           
138900     .                                                                    
139000 IMS-ISRT-5112 SECTION.                                                   
139100                                                                          
139200     MOVE SPACE TO GODK-STATUSKODER                                       
139300     CALL CBLTDLI USING ISRT ALT5112-PCB MSG-IO-AREA                      
139400     MOVE ALT5112-STATUS-CODE TO STATUS-WS                                
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     CALL CBLTDLI USING ISRT ALT5112-PCB MSG-KOM-WMSGKOM                  
139700     MOVE ALT5112-STATUS-CODE TO STATUS-WS                                
139800     PERFORM IMS-STATUSKONTROLL                                           
139900     .                                                                    
140000 IMS-ISRT-5119 SECTION.                                                   
140100                                                                          
140200     MOVE SPACE TO GODK-STATUSKODER                                       
140300     CALL CBLTDLI USING ISRT ALT5119-PCB MSG-IO-AREA                      
140400     MOVE ALT5119-STATUS-CODE TO STATUS-WS                                
140500     PERFORM IMS-STATUSKONTROLL                                           
140600     CALL CBLTDLI USING ISRT ALT5119-PCB MSG-KOM-WMSGKOM                  
140700     MOVE ALT5119-STATUS-CODE TO STATUS-WS                                
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000 IMS-ISRT-5206 SECTION.                                                   
141100                                                                          
141200     MOVE SPACE TO GODK-STATUSKODER                                       
141300     CALL CBLTDLI USING ISRT ALT5206-PCB MSG-IO-AREA                      
141400     MOVE ALT5206-STATUS-CODE TO STATUS-WS                                
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     CALL CBLTDLI USING ISRT ALT5206-PCB MSG-KOM-WMSGKOM                  
141700     MOVE ALT5206-STATUS-CODE TO STATUS-WS                                
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000 IMS-ISRT-611B SECTION.                                                   
142100                                                                          
142200     MOVE SPACE TO GODK-STATUSKODER                                       
142300     CALL CBLTDLI USING ISRT ALT611B-PCB MSG-IO-AREA                      
142400     MOVE ALT611B-STATUS-CODE TO STATUS-WS                                
142500     PERFORM IMS-STATUSKONTROLL                                           
142600     CALL CBLTDLI USING ISRT ALT611B-PCB MSG-KOM-WMSGKOM                  
142700     MOVE ALT611B-STATUS-CODE TO STATUS-WS                                
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000 IMS-ISRT-611C SECTION.                                                   
143100                                                                          
143200     MOVE SPACE TO GODK-STATUSKODER                                       
143300     CALL CBLTDLI USING ISRT ALT611C-PCB MSG-IO-AREA                      
143400     MOVE ALT611C-STATUS-CODE TO STATUS-WS                                
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     CALL CBLTDLI USING ISRT ALT611C-PCB MSG-KOM-WMSGKOM                  
143700     MOVE ALT611C-STATUS-CODE TO STATUS-WS                                
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     EJECT                                                                
144100 IMS-ISRT-611D SECTION.                                                   
144200                                                                          
144300     MOVE SPACE TO GODK-STATUSKODER                                       
144400     CALL CBLTDLI USING ISRT ALT611D-PCB MSG-IO-AREA                      
144500     MOVE ALT611D-STATUS-CODE TO STATUS-WS                                
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     CALL CBLTDLI USING ISRT ALT611D-PCB MSG-KOM-WMSGKOM                  
144800     MOVE ALT611D-STATUS-CODE TO STATUS-WS                                
144900     PERFORM IMS-STATUSKONTROLL                                           
145000     .                                                                    
145100 IMS-ISRT-6192 SECTION.                                                   
145200                                                                          
145300     MOVE SPACE TO GODK-STATUSKODER                                       
145400     CALL CBLTDLI USING ISRT ALT6192-PCB MSG-IO-AREA                      
145500     MOVE ALT6192-STATUS-CODE TO STATUS-WS                                
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     CALL CBLTDLI USING ISRT ALT6192-PCB MSG-KOM-WMSGKOM                  
145800     MOVE ALT6192-STATUS-CODE TO STATUS-WS                                
145900     PERFORM IMS-STATUSKONTROLL                                           
146000     .                                                                    
146100     EJECT                                                                
146200 IMS-ISRT-6193 SECTION.                                                   
146300                                                                          
146400     MOVE SPACE TO GODK-STATUSKODER                                       
146500     CALL CBLTDLI USING ISRT ALT6193-PCB MSG-IO-AREA                      
146600     MOVE ALT6193-STATUS-CODE TO STATUS-WS                                
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     CALL CBLTDLI USING ISRT ALT6193-PCB MSG-KOM-WMSGKOM                  
146900     MOVE ALT6193-STATUS-CODE TO STATUS-WS                                
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200 IMS-ISRT-619B SECTION.                                                   
147300                                                                          
147400     MOVE SPACE TO GODK-STATUSKODER                                       
147500     CALL CBLTDLI USING ISRT ALT619B-PCB MSG-IO-AREA                      
147600     MOVE ALT619B-STATUS-CODE TO STATUS-WS                                
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     CALL CBLTDLI USING ISRT ALT619B-PCB MSG-KOM-WMSGKOM                  
147900     MOVE ALT619B-STATUS-CODE TO STATUS-WS                                
148000     PERFORM IMS-STATUSKONTROLL                                           
148100     .                                                                    
148200 IMS-ISRT-Z430 SECTION.                                                   
148300                                                                          
148400     MOVE SPACE TO GODK-STATUSKODER                                       
148500     CALL CBLTDLI USING ISRT ALTZ430-PCB MSG-IO-AREA                      
148600     MOVE ALTZ430-STATUS-CODE TO STATUS-WS                                
148700     PERFORM IMS-STATUSKONTROLL                                           
148800                                                                          
148900     CALL CBLTDLI USING ISRT ALTZ430-PCB MSG-KOM-WMSGKOM                  
149000     MOVE ALTZ430-STATUS-CODE TO STATUS-WS                                
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300 IMS-GET-KOMA-ROT SECTION.                                                
149400                                                                          
149500     STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                        
149600          DELIMITED BY SIZE INTO SSA1                                     
149700     MOVE '  GE' TO GODK-STATUSKODER                                      
149800     CALL CBLTDLI USING GHU KOMA-PCB DLI-IO-KOMA01 SSA1                   
149900     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200 IMS-GET-KOMA-TRANS SECTION.                                              
150300                                                                          
150400     MOVE 'WLKOMA11 ' TO SSA1                                             
150500     MOVE '  GE' TO GODK-STATUSKODER                                      
150600     CALL CBLTDLI USING GHNP KOMA-PCB DLI-IO-KOMA11 SSA1                  
150700     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
150800     PERFORM IMS-STATUSKONTROLL                                           
150900     .                                                                    
151000 IMS-REPL-KOMA SECTION.                                                   
151100                                                                          
151200     MOVE '  ' TO GODK-STATUSKODER                                        
151300     CALL CBLTDLI USING REPL KOMA-PCB DLI-IO-KOMA01                       
151400     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700 IMS-DLET-KOMA SECTION.                                                   
151800                                                                          
151900     MOVE '  ' TO GODK-STATUSKODER                                        
152000     CALL CBLTDLI USING DLET KOMA-PCB DLI-IO-KOMA11                       
152100     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     .                                                                    
152400 IMS-STATUSKONTROLL SECTION.                                              
152500                                                                          
152600     SET STATUS-IX TO 1                                                   
152700     SEARCH GODK-STATUS                                                   
152800       AT END                                                             
152900         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
153000           DELIMITED BY SIZE INTO FELTEXT                                 
153100         CALL FELLOG                                                      
153200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
153300         CONTINUE                                                         
153400     END-SEARCH                                                           
153500     .                                                                    
