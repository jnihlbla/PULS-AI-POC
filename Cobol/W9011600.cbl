000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9011600.                                                
000400 AUTHOR.         SKOGLUND LENA.                                           
000500 DATE-WRITTEN.   03/05/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       CARPARTS.PULS.TACDISSALDO                                
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        SALDOFRÅGA - TACDIS                                              
001200*        ARTIKELNR + ANTAL  KONTOLLERAS MOT W411SDCA                      
001300*        ARTIKELNR + ANTAL  KONTOLLERAS MOT W411CDCA                      
001400*                                                                         
001500*        ÄNDRAT HÖSTEN 2018 FÖR ATT FÅ GENSAM LOGIK                       
001600*        MED W9011600 OCH W9033100 GENOM ANROP AV W911SLDO                
001700*        DÄR KONTROLL OM SLADO FINNS GÖRS                                 
001800*                                                                         
001900*    INDATA.                                                              
002000*        REQUEST:     W90116I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESPONSE:    W90116O1                                            
002400*                                                                         
002500*        PROGRAMMET LÄSER      WDB2  KUNDREG                              
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400                                                                          
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W9011600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004800 77  WS-FELTEXT                  PIC X(36)   VALUE SPACE.                 
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 01    WS-TIDISPIN               PIC 9(7)    VALUE ZERO.                  
005500 01    FILLER REDEFINES WS-TIDISPIN.                                      
005600   03  FILLER                    PIC X.                                   
005700   03  WS-TIDISPIN-X             PIC X(6).                                
005800                                                                          
005900                                                                          
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERELLA-SUBPROGRAM.                                                
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
006700     03  W911SLDO                PIC X(8)    VALUE 'W911SLDO'.            
006800                                                                          
006900                                                                          
007000                                                                          
007100 01  WS-IDARTNR                  PIC S9(9)   COMP-3 VALUE ZERO.           
007200*01  WS-IDARTBET                 PIC X(17).                               
007300*01  FILLER REDEFINES WS-IDARTBET.                                        
007400*    03 WS-IDARTNR               PIC 9(9).                                
007500*    03 FILLER                   PIC X(8).                                
007600                                                                          
007700*    --- PARAMETRAR TILL ABEND                                            
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000                                                                          
008100 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'W009CIA-AREA'.        
008500*01  -COPY W009CIA                                                        
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008800*01  -COPY WZ01SUB                                                        
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'W911SLDO   '.         
009100*01  -COPY W911SLDO                                                       
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009400 01  REQU-AREA.                                                           
009500*    03  -COPY W90116I1 -PRE REQU-                                        
009600                                                                          
009700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009800 01  RESP-AREA.                                                           
009900*    03  -COPY W90116O1 -PRE RESP-                                        
010000*    03  RESP-TEST         PIC X(20) VALUE 'XXXXXXXXXXXXXXXXXXXX'.        
010100                                                                          
010200                                                                          
010300                                                                          
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700                                                                          
010800 01  NYCKLAR-TILL-DLI.                                                    
010900*    -NYCKLAR TIL WDB201                                                  
011000     03  W-IDGMT-X.                                                       
011100       05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO COMP-3.           
011200       05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO COMP-3.           
011300                                                                          
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700                                                                          
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000                                                                          
012100 01  SSA1                        PIC X(64).                               
012200                                                                          
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012500                                                                          
012600                                                                          
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800                                                                          
012900 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB201'.            
013000 01  DLI-IO-WDB201.                                                       
013100*     03  -COPY WDB201.                                                   
013200                                                                          
013300                                                                          
013400 LINKAGE SECTION.                                                         
013500 01  IO-PCB       PIC X.                                                  
013600                                                                          
013700*01  -COPY W0009  -PRE MSG-                                               
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE WDB2-                                              
014000     05  FILLER                  PIC X.                                   
014100                                                                          
014200 01  SLDO-WDF1-PCB               PIC X.                                   
014300 01  SLDO-WDF2-PCB               PIC X.                                   
014400 01  SLDO-WDF2A-PCB              PIC X.                                   
014500 01  SLDO-WDK7-PCB               PIC X.                                   
014600 01  SLDO-WDK6-PCB               PIC X.                                   
014700 01  SLDO-XXKJ-PCB               PIC X.                                   
014800 01  SLDO-WDD7-PCB               PIC X.                                   
014900 01  SLDO-BENA-PCB               PIC X.                                   
015000 01  SLDO-WDB3-PCB               PIC X.                                   
015100 01  SLDO-WDR6-PCB               PIC X.                                   
015200 01  SLDO-WDB2-PCB               PIC X.                                   
015300 01  SLDO-WDB1-PCB               PIC X.                                   
015400                                                                          
015500 01  SLDO-KVAN-WDB2-PCB          PIC X.                                   
015600 01  SLDO-KVAN-WDC1-PCB          PIC X.                                   
015700                                                                          
015800 01  SLDO-AREG-WDK6-PCB          PIC X.                                   
015900 01  SLDO-AREG-WDK7-PCB          PIC X.                                   
016000                                                                          
016100 01  SLDO-DLEV-LEVF-PCB          PIC X.                                   
016200 01  SLDO-DLEV-LEVG-PCB          PIC X.                                   
016300 01  SLDO-DLEV-LEVA-PCB          PIC X.                                   
016400 01  SLDO-DLEV-ARTS-PCB          PIC X.                                   
016500 01  SLDO-DLEV-WDB6-PCB          PIC X.                                   
016600 01  SLDO-DLEV-FILA-PCB          PIC X.                                   
016700                                                                          
016800 01  SLDO-SPAR-WDF8-PCB          PIC X.                                   
016900 01  SLDO-SPAR-WDF8A-PCB         PIC X.                                   
017000 01  SLDO-SPAR-WDK6-PCB          PIC X.                                   
017100                                                                          
017200 01  SLDO-SDCA-ARTS-PCB          PIC X.                                   
017300 01  SLDO-SDCA-WDB6-PCB          PIC X.                                   
017400 01  SLDO-SDCA-WDK9-PCB          PIC X.                                   
017500 01  SLDO-SDCA-WDR6-PCB          PIC X.                                   
017600 01  SLDO-SDCA-WDK6-PCB          PIC X.                                   
017700 01  SLDO-SDCA-WDQ4B-PCB         PIC X.                                   
017800 01  SLDO-SDCA-WDQ2-PCB          PIC X.                                   
017900 01  SLDO-SDCA-WDQ4-PCB          PIC X.                                   
018000 01  SLDO-SDCA-WDB6-2-PCB        PIC X.                                   
018100 01  SLDO-SDCA-WDK6-2-PCB        PIC X.                                   
018200 01  SLDO-SDCA-WDK7-2-PCB        PIC X.                                   
018300 01  SLDO-SDCA-WDK7-3-PCB        PIC X.                                   
018400                                                                          
018500 01  SLDO-NDCA-USEA-PCB          PIC X.                                   
018600 01  SLDO-NDCA-WDK7-PCB          PIC X.                                   
018700 01  SLDO-NDCA-WDL6-PCB          PIC X.                                   
018800 01  SLDO-NDCA-WDB6-PCB          PIC X.                                   
018900                                                                          
019000 01  SLDO-RANS-XXKM-PCB          PIC X.                                   
019100 01  SLDO-RANS-ARTM-PCB          PIC X.                                   
019200 01  SLDO-RANS-ARTS-PCB          PIC X.                                   
019300                                                                          
019400 01  SLDO-CDCA-ARTM-PCB          PIC X.                                   
019500 01  SLDO-CDCA-INLB-PCB          PIC X.                                   
019600 01  SLDO-CDCA-WDB2-PCB          PIC X.                                   
019700 01  SLDO-CDCA-WDC1-PCB          PIC X.                                   
019800                                                                          
019900 01  SLDO-CLDC-WDB6-PCB          PIC X.                                   
020000                                                                          
020100 01  SLDO-ETA-ARTC-PCB           PIC X.                                   
020200 01  SLDO-ETA-WDK7-PCB           PIC X.                                   
020300 01  SLDO-ETA-INLC-PCB           PIC X.                                   
020400 01  SLDO-ETA-LEVA-PCB           PIC X.                                   
020500 01  SLDO-ETA-WDB6-PCB           PIC X.                                   
020600 01  SLDO-ETA-WDD9-PCB           PIC X.                                   
020700                                                                          
020800 01  SLDO-XDCA-USEA-PCB          PIC X.                                   
020900 01  SLDO-XDCA-WDB6-PCB          PIC X.                                   
021000 01  SLDO-XDCA-WDK6-PCB          PIC X.                                   
021100 01  SLDO-XDCA-WDK7-PCB          PIC X.                                   
021200 01  SLDO-XDCA-WDK9-PCB          PIC X.                                   
021300 01  SLDO-XDCA-WDL6-PCB          PIC X.                                   
021400 01  SLDO-XDCA-WDQ4B-PCB         PIC X.                                   
021500 01  SLDO-XDCA-WDQ2-PCB          PIC X.                                   
021600 01  SLDO-XDCA-WDQ4-PCB          PIC X.                                   
021700 01  SLDO-XDCA-WDR6-PCB          PIC X.                                   
021800 01  SLDO-XDCA-WDB6-2-PCB        PIC X.                                   
021900 01  SLDO-XDCA-WDK6-2-PCB        PIC X.                                   
022000 01  SLDO-XDCA-WDK7-2-PCB        PIC X.                                   
022100 01  SLDO-XDCA-WDK7-3-PCB        PIC X.                                   
022200                                                                          
022300 PROCEDURE DIVISION  USING MSG-PCB WDB2-PCB                               
022400                                                                          
022500                           SLDO-WDF1-PCB SLDO-WDF2-PCB                    
022600                           SLDO-WDF2A-PCB SLDO-WDK7-PCB                   
022700                           SLDO-WDK6-PCB                                  
022800                           SLDO-XXKJ-PCB SLDO-WDD7-PCB                    
022900                           SLDO-BENA-PCB SLDO-WDB3-PCB                    
023000                           SLDO-WDR6-PCB                                  
023100                           SLDO-WDB2-PCB SLDO-WDB1-PCB                    
023200                                                                          
023300                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
023400                                                                          
023500                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
023600                                                                          
023700                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
023800                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
023900                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
024000                                                                          
024100                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
024200                           SLDO-SPAR-WDK6-PCB                             
024300                                                                          
024400                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
024500                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
024600                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
024700                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
024800                           SLDO-SDCA-WDB6-2-PCB                           
024900                           SLDO-SDCA-WDK6-2-PCB                           
025000                           SLDO-SDCA-WDK7-2-PCB                           
025100                           SLDO-SDCA-WDK7-3-PCB                           
025200                                                                          
025300                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
025400                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
025500                                                                          
025600                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
025700                           SLDO-RANS-ARTS-PCB                             
025800                                                                          
025900                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
026000                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
026100                                                                          
026200                           SLDO-CLDC-WDB6-PCB                             
026300                                                                          
026400                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
026500                           SLDO-ETA-INLC-PCB  SLDO-ETA-LEVA-PCB           
026600                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
026700                                                                          
026800                           SLDO-XDCA-USEA-PCB                             
026900                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
027000                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
027100                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
027200                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
027300                           SLDO-XDCA-WDR6-PCB                             
027400                           SLDO-XDCA-WDB6-2-PCB                           
027500                           SLDO-XDCA-WDK6-2-PCB                           
027600                           SLDO-XDCA-WDK7-2-PCB                           
027700                           SLDO-XDCA-WDK7-3-PCB.                          
027800 MAIN SECTION.                                                            
027900     ENTRY 'DLITCBL' USING MSG-PCB WDB2-PCB                               
028000                                                                          
028100                           SLDO-WDF1-PCB SLDO-WDF2-PCB                    
028200                           SLDO-WDF2A-PCB SLDO-WDK7-PCB                   
028300                           SLDO-WDK6-PCB                                  
028400                           SLDO-XXKJ-PCB SLDO-WDD7-PCB                    
028500                           SLDO-BENA-PCB SLDO-WDB3-PCB                    
028600                           SLDO-WDR6-PCB                                  
028700                           SLDO-WDB2-PCB SLDO-WDB1-PCB                    
028800                                                                          
028900                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
029000                                                                          
029100                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
029200                                                                          
029300                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
029400                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
029500                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
029600                                                                          
029700                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
029800                           SLDO-SPAR-WDK6-PCB                             
029900                                                                          
030000                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
030100                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
030200                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
030300                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
030400                           SLDO-SDCA-WDB6-2-PCB                           
030500                           SLDO-SDCA-WDK6-2-PCB                           
030600                           SLDO-SDCA-WDK7-2-PCB                           
030700                           SLDO-SDCA-WDK7-3-PCB                           
030800                                                                          
030900                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
031000                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
031100                                                                          
031200                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
031300                           SLDO-RANS-ARTS-PCB                             
031400                                                                          
031500                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
031600                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
031700                                                                          
031800                           SLDO-CLDC-WDB6-PCB                             
031900                                                                          
032000                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
032100                           SLDO-ETA-INLC-PCB  SLDO-ETA-LEVA-PCB           
032200                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
032300                                                                          
032400                           SLDO-XDCA-USEA-PCB                             
032500                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
032600                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
032700                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
032800                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
032900                           SLDO-XDCA-WDR6-PCB                             
033000                           SLDO-XDCA-WDB6-2-PCB                           
033100                           SLDO-XDCA-WDK6-2-PCB                           
033200                           SLDO-XDCA-WDK7-2-PCB                           
033300                           SLDO-XDCA-WDK7-3-PCB.                          
033400                                                                          
033500     PERFORM A-INIT                                                       
033600     PERFORM S01-HAEMTA-ANROPSDATA                                        
033700     IF SUB-KDRC = 0                                                      
033800       PERFORM B-KOLLA-NYCKLAR                                            
033900       IF NYCKLAR-OK                                                      
034000         PERFORM C-EXECUTE                                                
034100       END-IF                                                             
034200       PERFORM S02-RETURNERA-SVAR                                         
034300     END-IF                                                               
034400     MOVE ZERO                     TO RETURN-CODE                         
034500     GOBACK                                                               
034600     .                                                                    
034700                                                                          
034800                                                                          
034900 A-INIT SECTION.                                                          
035000     MOVE 'A-INIT          '       TO CURRENT-SECTION                     
035100                                                                          
035200     MOVE YES                      TO RESP-KDSVAR                         
035300     MOVE SPACE                    TO RESP-IDDC                           
035400     MOVE NEJ                      TO RESP-FLDIRLEV                       
035500     MOVE SPACE                    TO RESP-TIDISPIN                       
035600     MOVE NEJ                      TO RESP-FLERSATT                       
035700     .                                                                    
035800                                                                          
035900 B-KOLLA-NYCKLAR SECTION.                                                 
036000     MOVE 'B-KOLLA-NYCKLAR '       TO CURRENT-SECTION                     
036100                                                                          
036200     MOVE JA                       TO NYCKLAR-SW                          
036300     IF REQU-IDDISTR   NOT NUMERIC                                        
036400       MOVE NEJ                    TO NYCKLAR-SW                          
036500     END-IF                                                               
036600                                                                          
036700     IF REQU-IDKUNDNR  NOT NUMERIC                                        
036800       MOVE NEJ                    TO NYCKLAR-SW                          
036900     END-IF                                                               
037000                                                                          
037100     MOVE 'VO '                    TO CIA-IDARTPRE-IN                     
037200     MOVE REQU-IDARTBET            TO CIA-IDARTBET-IN                     
037300     CALL W009CIA               USING CIA-W009CIA                         
037400     IF CIA-KDSVAR NOT = 'F'                                              
037500       MOVE CIA-IDARTNR            TO WS-IDARTNR                          
037600     ELSE                                                                 
037700       MOVE NEJ                    TO NYCKLAR-SW                          
037800     END-IF                                                               
037900                                                                          
038000     IF REQU-KVBEART-Q NOT  NUMERIC                                       
038100       MOVE NEJ                    TO NYCKLAR-SW                          
038200     END-IF                                                               
038300* MAN KAN INTE FRÅGA PÅ KVBEART = 0 FÖR ABENDAR W411CDCA                  
038400     IF REQU-KVBEART-Q = ZERO                                             
038500       MOVE NEJ                    TO NYCKLAR-SW                          
038600     END-IF                                                               
038700                                                                          
038800*    KOLLA OM FRÅGAN GÄLLER LDCKUND                                       
038900     MOVE REQU-IDDISTR             TO W-IDDISTR-WDB2                      
039000     MOVE REQU-IDKUNDNR            TO W-IDKUNDNR-WDB2                     
039100     PERFORM IMS-GU-WDB201                                                
039200     IF SEGMENT-FINNS                                                     
039300        IF GMT-FLLDCKND = JA                                              
039400           CONTINUE                                                       
039500        ELSE                                                              
039600           MOVE NEJ                TO NYCKLAR-SW                          
039700        END-IF                                                            
039800     ELSE                                                                 
039900        MOVE NEJ                   TO NYCKLAR-SW                          
040000     END-IF                                                               
040100                                                                          
040200     IF NYCKLAR-FEL                                                       
040300       MOVE 'E'                    TO RESP-KDSVAR                         
040400     END-IF                                                               
040500     .                                                                    
040600                                                                          
040700                                                                          
040800 C-EXECUTE SECTION.                                                       
040900     MOVE 'C-EXECUTE       '       TO CURRENT-SECTION                     
041000                                                                          
041100     MOVE WS-IDARTNR       TO SLDO-IDARTNR-IN                             
041200     MOVE REQU-IDDISTR     TO SLDO-IDDISTR-IN                             
041300     MOVE REQU-IDKUNDNR    TO SLDO-IDKUNDNR-IN                            
041400     MOVE 1                TO SLDO-KDORDKL-IN                             
041500     MOVE REQU-KVBEART-Q   TO SLDO-KVBEART-IN                             
041600     MOVE ZERO             TO SLDO-KVAVBART                               
041700                              SLDO-TIDISPIN                               
041800                              SLDO-KDORDBEK                               
041900                              SLDO-KVFRYSTI                               
042000     MOVE SPACE            TO SLDO-IDDC                                   
042100                              SLDO-IDMFSMED                               
042200                              SLDO-FLTPO1                                 
042300                                                                          
042400     CALL W911SLDO USING SLDO-W911SLDO                                    
042500                                                                          
042600                         SLDO-WDF1-PCB SLDO-WDF2-PCB                      
042700                         SLDO-WDF2A-PCB SLDO-WDK7-PCB                     
042800                         SLDO-WDK6-PCB                                    
042900                         SLDO-XXKJ-PCB SLDO-WDD7-PCB                      
043000                         SLDO-BENA-PCB SLDO-WDB3-PCB                      
043100                         SLDO-WDR6-PCB                                    
043200                         SLDO-WDB2-PCB SLDO-WDB1-PCB                      
043300                                                                          
043400                         SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB            
043500                                                                          
043600                         SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB            
043700                                                                          
043800                         SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB            
043900                         SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB            
044000                         SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB            
044100                                                                          
044200                         SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB           
044300                         SLDO-SPAR-WDK6-PCB                               
044400                                                                          
044500                         SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB            
044600                         SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB            
044700                         SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB           
044800                         SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB            
044900                         SLDO-SDCA-WDB6-2-PCB                             
045000                         SLDO-SDCA-WDK6-2-PCB                             
045100                         SLDO-SDCA-WDK7-2-PCB                             
045200                         SLDO-SDCA-WDK7-3-PCB                             
045300                                                                          
045400                         SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB            
045500                         SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB            
045600                                                                          
045700                         SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB            
045800                         SLDO-RANS-ARTS-PCB                               
045900                                                                          
046000                         SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB            
046100                         SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB            
046200                                                                          
046300                         SLDO-CLDC-WDB6-PCB                               
046400                                                                          
046500                         SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB             
046600                         SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB             
046700                         SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB             
046800                                                                          
046900                         SLDO-XDCA-USEA-PCB                               
047000                         SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB            
047100                         SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB            
047200                         SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB           
047300                         SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB            
047400                         SLDO-XDCA-WDR6-PCB                               
047500                         SLDO-XDCA-WDB6-2-PCB                             
047600                         SLDO-XDCA-WDK6-2-PCB                             
047700                         SLDO-XDCA-WDK7-2-PCB                             
047800                         SLDO-XDCA-WDK7-3-PCB                             
047900                                                                          
048000     IF SLDO-KDORDBEK = 95                                                
048100        MOVE YES        TO RESP-FLDIRLEV                                  
048200     END-IF                                                               
048300                                                                          
048400     IF SLDO-IDMFSMED = 'B10'                                             
048500*       FEL FRÅN W411KREG                                                 
048600        MOVE 'E'        TO RESP-KDSVAR                                    
048700     ELSE                                                                 
048800        IF SLDO-IDMFSMED = '080'                                          
048900*          KAN INTE LEVERERA                                              
049000           MOVE NEJ     TO RESP-KDSVAR                                    
049100           IF SLDO-KDORDBEK > 40 AND < 50                                 
049200              MOVE YES  TO RESP-FLERSATT                                  
049300           END-IF                                                         
049400           IF SLDO-TIDISPIN> ZERO                                         
049500              MOVE SLDO-TIDISPIN TO WS-TIDISPIN                           
049600              MOVE WS-TIDISPIN-X TO RESP-TIDISPIN                         
049700           END-IF                                                         
049800        ELSE                                                              
049900           IF (SLDO-KDORDBEK = ZERO OR 15 OR 95 OR 67) AND                
050000               SLDO-KVAVBART > ZERO                                       
050100*             KAN LEVERERA                                                
050200*             00 = FRÅN EGET LAGER                                        
050300*             15 = FRÅN CDC                                               
050400*             95 = FRÅN DIREKTLEVERANTÖR                                  
050500                                                                          
050600              MOVE YES        TO RESP-KDSVAR                              
050700              MOVE SLDO-IDDC  TO RESP-IDDC                                
050800           ELSE                                                           
050900              IF SLDO-KDORDBEK = 55 OR 58                                 
051000*             55 = MARKNADSSPÄRR                                          
051100*             58 = ARTIKEL OKÄND                                          
051200                 MOVE NEJ     TO RESP-KDSVAR                              
051300              ELSE                                                        
051400*             HÄR HAR VI ETT FAL SOM INTE HANTERAS                        
051500                 MOVE 'KOLLA VILKET SVAR SOM SKALL GES'                   
051600                                 TO WS-FELTEXT                            
051700                 MOVE NEJ        TO RESP-KDSVAR                           
051800*                CALL FELLOG                                              
051900*     DISPLAY 'W90116 SAKNAR HANTERING AV OBKR ' SLDO-KDORDBEK            
052000              END-IF                                                      
052100           END-IF                                                         
052200        END-IF                                                            
052300     END-IF                                                               
052400     .                                                                    
052500                                                                          
052600                                                                          
052700                                                                          
052800 S01-HAEMTA-ANROPSDATA SECTION.                                           
052900     MOVE 'S01-HAEMTA-ANROP'       TO CURRENT-SECTION                     
053000                                                                          
053100     MOVE 'GETARG'                 TO SUB-KDFUNC                          
053200     MOVE 'CARPARTS.PULS.TACDISSALDO'                                     
053300                                   TO SUB-ADDISPABS                       
053400     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
053500                                                                          
053600     CALL WZ01SUB               USING SUB-CONTROL-AREA                    
053700                                      SUB-KVDLEN                          
053800                                      REQU-AREA                           
053900                                                                          
054000     IF SUB-KDRC > 0 AND SUB-KDRC NOT = 20                                
054100       MOVE SUB-KDRC               TO KDRC-DISPLAY                        
054200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
054300       DELIMITED BY SIZE         INTO FELTEXT                             
054400       CALL ABEND               USING RKOD-ABEND-MED-DUMP                 
054500     END-IF                                                               
054600     .                                                                    
054700                                                                          
054800 S02-RETURNERA-SVAR SECTION.                                              
054900     MOVE 'S02-RETURSN-SVAR'       TO CURRENT-SECTION                     
055000                                                                          
055100     MOVE 'RETURN'                 TO SUB-KDFUNC                          
055200     MOVE LENGTH OF RESP-AREA      TO SUB-KVDLEN                          
055300                                                                          
055400     CALL WZ01SUB               USING SUB-CONTROL-AREA                    
055500                                      SUB-KVDLEN                          
055600                                      RESP-AREA                           
055700                                                                          
055800     IF SUB-KDRC > 0                                                      
055900       MOVE SUB-KDRC               TO KDRC-DISPLAY                        
056000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
056100       DELIMITED BY SIZE         INTO FELTEXT                             
056200       CALL ABEND               USING RKOD-ABEND-MED-DUMP                 
056300     END-IF                                                               
056400     .                                                                    
056500                                                                          
056600 IMS-GU-WDB201              SECTION.                                      
056700     MOVE 'IMS-GU-WDB201   '       TO CURRENT-IMS-SECTION                 
056800                                                                          
056900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
057000          DELIMITED BY SIZE INTO SSA1                                     
057100     MOVE '  GE'              TO GODK-STATUSKODER                         
057200                                                                          
057300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
057400     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700                                                                          
057800 IMS-STATUSKONTROLL SECTION.                                              
057900                                                                          
058000     SET STATUS-IX TO 1                                                   
058100     SEARCH GODK-STATUS                                                   
058200       AT END                                                             
058300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
058400         DELIMITED BY SIZE INTO FELTEXT                                   
058500         CALL FELLOG                                                      
058600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
058700         CONTINUE                                                         
058800     END-SEARCH                                                           
058900     .                                                                    
