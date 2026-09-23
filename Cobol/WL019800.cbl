000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019800.                                                
000300 AUTHOR.         CAP GEMINI AB/EP.                                        
000400 DATE-WRITTEN.   JUN   18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
001000*    FUNKTION.                                                            
001100*        PROGRAMMET UPPDATERAR URSPRUNG I ORDERRADEN FÖR                  
001200*        VISSA DISTRIKT. ENDAST AVVIKELSE MOT PACKUNDERLAG                
001300*        SKALL RAPPORTERAS.                                               
001400                                                                          
001500*  WL019800 PROGRAM IS A REPLICA OF W4031700 PROGRAM                      
001600*  AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                               
001700*                                                                         
001800*    NYCKELALTERNATIV.                                                    
001900*        PACKARE + ORDERID.                                               
002000*                  ORDERID: - DISTRIKT + KUNDNR + ORDERNR                 
002100*                           - PRODUKTIONSNUMMER                           
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0198U                                             
002400*        REQUEST:     WL0198I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WL0198O1                                            
002800*                                                                         
002900*                                                                         
003000*    EJECT                                                                
003100                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77    IDPGM                     PIC X(8)    VALUE 'WL019800'.            
004000 77    JA                        PIC X       VALUE 'J'.                   
004100 77    YES                       PIC X       VALUE 'Y'.                   
004200 77    NEJ                       PIC X       VALUE 'N'.                   
004300 77    RAETT                     PIC X       VALUE 'R'.                   
004400 77    FEL                       PIC X       VALUE 'F'.                   
004500 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
004600 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77    ANT-BILD-RADER            PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +267 COMP SYNC.        
005100 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +100 COMP SYNC.        
005200 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
005300 77    WS-IDDC                   PIC X(2).                                
005400 77    WS-IDDC-NUM               PIC 9(2).                                
005500 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
005600 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
005700 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
005800 77    WS-IDORDNR                PIC X(5)   VALUE SPACE.                  
005900 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
006000 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
006100 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
006200 77    WS-IDRADNR                PIC 9(4)   VALUE ZERO.                   
006300 77    WS-KDARTURS               PIC X(2)   VALUE SPACE.                  
006400 77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5)  VALUE ZERO COMP-3.            
006500 77    MAX-RAD-ANTAL             PIC S9(3)  VALUE +15  COMP-3.            
006600 77    WS-TRAEFF-PACKARE         PIC X(01).                               
006700 77    WS-TRAEFF-RAD             PIC X(01).                               
006900     SKIP2                                                                
007000 77    WS-SLINGA-KLAR            PIC X(01).                               
007100   88  SLINGA-KLAR                          VALUE 'J'.                    
007200 77    VISA-UPPDATERAT-MED-SW    PIC X(01).                               
007300   88  VISA-UPPDATERAT-MED                  VALUE 'J'.                    
007400 77    WS-INDATA-TEST            PIC X(01).                               
007500   88  WS-INDATA-FEL                        VALUE 'F'.                    
007600   88  WS-INDATA-RATT                       VALUE 'R'.                    
007700 77    WS-IDELMT-ERROR           PIC X(16).                               
007800 77    WS-IDMSG-ERROR            PIC X(03).                               
007900 77    WS-IDMSG-INFO             PIC X(03).                               
007910 77    WS-COUNT                  PIC 9(3)   VALUE ZERO.                   
007911 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
007912 77    KDRC-DISPLAY              PIC Z(5).                                
007920 01    ALL-PLUS.                                                          
007930   03 FILLER                     PIC X(30)  VALUE                         
007940        '++++++++++++++++++++++++++++++'.                                 
007950*    --- PARAMETERS TO ABEND                                              
007960                                                                          
007970 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007980 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007990 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007991                                                                          
007992                                                                          
008000     SKIP2                                                                
008100 01  FELMEDDELANDEN.                                                      
008210   03    FEL-DEV-IN-PROGRESS     PIC  X(03)  VALUE '419'.                 
008310   03    FEL-LINE-NOT-BELONG-TO-PICK  PIC  X(03)  VALUE '420'.            
008410   03    FEL-INVALID-DATA        PIC  X(03)  VALUE '421'.                 
008610   03    FEL-ORDER-MISSING       PIC  X(03)  VALUE '304'.                 
008900   03    SYSTEM-ERROR            PIC  X(03)  VALUE '099'.                 
009400   03    FEL-WRONG-PICKER        PIC  X(03)  VALUE '418'.                 
010600                                                                          
010700 01  MESSAGE-CODES.                                                       
010800     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
010900                                                                          
011000 01    DYNAMISKA-SUBPGM.                                                  
011100    03 CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
011200    03 FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
011300    03 W400ARTU                  PIC X(8)   VALUE 'W400ARTU'.             
011410    03 WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
011420    03 ABEND                     PIC X(8)   VALUE 'ABEND   '.             
011500                                                                          
011600 01    FILLER                    PIC X(16) VALUE 'W400ARTU-AREA'.         
011700*01    FILLER  -COPY W400ARTU                                             
012300     EJECT                                                                
012400 01    WS-JFR-IDANSTNR.                                                   
012500   03  FILLER                    PIC X(3).                                
012600   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
012700     SKIP2                                                                
012800*01    NYCKLAR-TILL-DLI.                                                  
012900*                                                                         
013000 01  NYCKLAR-TILL-DLI.                                                    
013100   03    W-WDE601KY-X.                                                    
013200     05    W-IDPRODNR-WDE6       PIC S9(7)   VALUE ZERO  COMP-3.          
013300                                                                          
013400   03    W-WDE4F1KY-MAX-X.                                                
013500     05    W-IDPRODNR-WDE4F-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
013600     05    W-IDKOLLI-WDE4F-MAX   PIC S9(5)   VALUE ZERO  COMP-3.          
013700     05    W-WDE4F1-MAX          PIC X(22)   VALUE HIGH-VALUE.            
013800                                                                          
013900   03    W-WDE4F1KY-MIN-X.                                                
014000     05    W-IDPRODNR-WDE4F-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
014100     05    W-IDKOLLI-WDE4F-MIN   PIC S9(5)   VALUE ZERO  COMP-3.          
014200     05    W-WDE4F1-MIN          PIC X(22)   VALUE LOW-VALUE.             
014300                                                                          
014400   03    W-WDE4A1-KUNDORDER-X.                                            
014500     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
014600     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
014700     05    W-4A1-IDKUNDRF.                                                
014800       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
014900       07  FILLER                PIC X(05)   VALUE SPACE.                 
015000*                                                                         
015100   03    W-WDE401-KUNDORDER-X.                                            
015200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
015300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
015400     05    W-401-IDKUNDRF.                                                
015500       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
015600       07  FILLER                PIC X(05)   VALUE SPACE.                 
015700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
015800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
015900*                                                                         
016000   03    W-WDE4B-KEYSEQ-X.                                                
016100     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
016200     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
016300*                                                                         
016400   03    W-WDE420-KEYSEQ-X.                                               
016500     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
016600*                                                                         
016700*                                                                         
016800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016900     SKIP3                                                                
017000*01  -COPY WZ01SUB                                                        
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017300     SKIP3                                                                
017400 01  REQU-AREA.                                                           
017500*    03  -COPY WZ01REQU                                                   
017600*    03  -COPY WL0198I1                                                   
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017900     SKIP3                                                                
018000 01  RESP-AREA.                                                           
018100*    03  -COPY WZ01RESP                                                   
018200*    03  -COPY WL0198O1                                                   
018300     EJECT                                                                
018400*                                                                         
018500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018600*                                                                         
018700 01    IMS-WS.                                                            
018800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
018900     SKIP3                                                                
019000*                        **** STATUS-KOD FRÅN IMS                         
019100   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
019200     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
019300     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
019400   03    STATUS-WS               PIC XX.                                  
019500     88    SEGMENT-FINNS                     VALUE '  '.                  
019600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
019700     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
019800     SKIP3                                                                
019900   03    GODK-STATUSKODER.                                                
020000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
020100     SKIP3                                                                
020200 01    SSA1                      PIC X(64).                               
020300 01    SSA2                      PIC X(64).                               
020400 01    SSA3                      PIC X(64).                               
020500 01    SSA4                      PIC X(64).                               
020600     EJECT                                                                
020700*                            IMS FUNKTIONSKODER                           
020800*01    -COPY W0003                                                        
020900     EJECT                                                                
021000*                            DLI INPUT-OUTPUT AREA                        
021100 01    DLI-IO-AREA.                                                       
021200   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
021300     SKIP3                                                                
021400*  03    WDE401   -COPY WDE401             -RED IO-AREA.                  
021500     EJECT                                                                
021600*  03    WDE411   -COPY WDE411             -RED IO-AREA.                  
021700     EJECT                                                                
021800*  03    WDE601   -COPY WDE601             -RED IO-AREA.                  
021900     EJECT                                                                
022000 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE4F1'.               
022100 01  DLI-IO-WDE4F1.                                                       
022200*    03  -COPY WDE4F1  -PRE WDE4F-                                        
022300 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE401'.               
022400 01  DLI-IO-WDE401.                                                       
022500*    03  -COPY WDE401  -PRE WDE4-                                         
022600 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE601'.               
022700 01  DLI-IO-WDE601.                                                       
022800*    03  -COPY WDE601  -PRE WDE6-                                         
022900     EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100*01    -COPY W0009     -PRE MSG-                                          
023200     EJECT                                                                
023300*01    -COPY W0008     -PRE WDE4-                                         
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01    -COPY W0008     -PRE WDE4A-                                        
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01    -COPY W0008     -PRE WDE4B-                                        
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200*01    -COPY W0008     -PRE WDE6-                                         
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500 PROCEDURE DIVISION USING  MSG-PCB                                        
024600                           WDE4-PCB WDE4A-PCB                             
024700                           WDE4B-PCB WDE6-PCB.                            
024800 MAIN SECTION.                                                            
024900     ENTRY 'DLITCBL' USING MSG-PCB                                        
025000                           WDE4-PCB WDE4A-PCB                             
025100                           WDE4B-PCB WDE6-PCB.                            
025200                                                                          
025300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
025310                                                                          
025400     IF SUB-KDRC = 0                                                      
025500       IF REQU-KDPGMACT = 'E' OR 'S'                                      
025600          PERFORM A-INIT                                                  
025700          PERFORM B-GENERELL-KONTROLL                                     
025800*                                                                         
025900          IF WS-INDATA-RATT                                               
026000              PERFORM C-RELATIONSKONTROLL                                 
026100*                                                                         
026200              IF WS-INDATA-RATT                                           
026300                  MOVE +1 TO INX                                          
026400                  PERFORM UNTIL INX > MAX-RAD-ANTAL                       
026500                      PERFORM D-BEHANDLA-RAD                              
026600                      ADD +1 TO INX                                       
026700                  END-PERFORM                                             
026800              END-IF                                                      
026900          END-IF                                                          
027000                                                                          
027100          IF WS-INDATA-RATT                                               
027200              PERFORM H-AVSLUT                                            
027300          END-IF                                                          
027400       ELSE                                                               
027500          MOVE SYSTEM-ERROR     TO RESP-IDMSG-ERROR                       
027600       END-IF                                                             
027700                                                                          
027800       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
027900       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
028000       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
028100                                                                          
028200       IF WS-IDMSG-ERROR NOT = SPACE                                      
028300           MOVE ALL '+' TO RESP-WL0198O1(1:34)                            
028400           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
028500           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
028600           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
028700           MOVE 001              TO RESP-IDMSGVER                         
028800           MOVE WS-COUNT         TO RESP-KVRADER                          
028900       END-IF                                                             
029000                                                                          
029100       PERFORM S02-RETURN-RESPONSE                                        
029200     END-IF                                                               
029300     MOVE ZERO TO RETURN-CODE                                             
029301                                                                          
029400     GOBACK                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 A-INIT             SECTION.                                              
029800                                                                          
029900     MOVE ALL '+'                         TO RESP-AREA                    
030000     MOVE 001                             TO RESP-IDMSGVER                
030100     MOVE SPACE                           TO RESP-IDMSG-ERROR             
030200                                             RESP-IDMSG-INFO              
030300                                             RESP-IDELMT-ERROR            
030400     MOVE ZERO                            TO WS-COUNT                     
030500     MOVE ZERO                            TO RESP-KVRADER                 
030600     MOVE RAETT                           TO WS-INDATA-TEST               
030800                                                                          
030900*    NEDAN   RAD   SKA VARA MED? BYT TILL RESP                            
031000***  MOVE MID-IDTRANS-START               TO   MOD-IDTRANS-START          
031200*                                                                         
031300     MOVE RAETT                          TO WS-INDATA-TEST                
031400     MOVE NEJ                            TO VISA-UPPDATERAT-MED-SW        
031500                                                                          
031600     PERFORM AA-FLYTTA-NYCKLAR                                            
032500     .                                                                    
032600     EJECT                                                                
032700 AA-FLYTTA-NYCKLAR  SECTION.                                              
032800                                                                          
032900     IF REQU-L198-IDANSTNR-KEY = ALL '+'                                  
033000      CONTINUE                                                            
033100     ELSE                                                                 
033200         MOVE REQU-L198-IDANSTNR-KEY      TO   WS-IDANSTNR                
033300     END-IF                                                               
033400     SKIP2                                                                
033500     IF REQU-L198-IDPRODNR-KEY = ALL '+'                                  
033600        MOVE ZERO                         TO  WS-IDPRODNR                 
033700     ELSE                                                                 
033800         MOVE REQU-L198-IDPRODNR-KEY      TO   WS-IDPRODNR                
033900     END-IF                                                               
034000     SKIP2                                                                
034100     IF REQU-L198-IDDISTR-KEY = ALL '+'                                   
034200        MOVE ZERO                         TO WS-IDDISTR                   
034300     ELSE                                                                 
034400         MOVE REQU-L198-IDDISTR-KEY       TO   WS-IDDISTR                 
034500     END-IF                                                               
034600     SKIP2                                                                
034700     IF REQU-L198-IDKUNDNR-KEY = ALL '+'                                  
034800       MOVE ZERO                       TO   WS-IDKUNDNR                   
034900     ELSE                                                                 
035000         MOVE REQU-L198-IDKUNDNR-KEY      TO   WS-IDKUNDNR                
035100     END-IF                                                               
035200     SKIP2                                                                
035300     IF REQU-L198-IDORDNR-KEY = ALL '+'                                   
035400        MOVE ZERO                         TO WS-IDORDNR                   
035500     ELSE                                                                 
035600         MOVE REQU-L198-IDORDNR-KEY       TO   WS-IDORDNR                 
035700     END-IF                                                               
035800     SKIP2                                                                
035900     IF  REQU-L198-IDKOLLI-KEY = ALL '+'                                  
036000       CONTINUE                                                           
036100     ELSE                                                                 
036200         MOVE REQU-L198-IDKOLLI-KEY       TO   WS-IDKOLLI                 
036300     END-IF                                                               
036400                                                                          
036500     IF  WS-IDANSTNR NUMERIC                                              
036600     MOVE WS-IDANSTNR                     TO   RESP-IDANSTNR-KEY          
036700     INSPECT RESP-IDANSTNR-KEY REPLACING LEADING ZERO BY SPACE            
036800     END-IF                                                               
036900                                                                          
037000     IF WS-IDDISTR NUMERIC                                                
037100     MOVE WS-IDDISTR                      TO   RESP-IDDISTR-KEY           
037200     INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE            
037300     END-IF                                                               
037400                                                                          
037500     IF WS-IDKUNDNR NUMERIC                                               
037600     MOVE WS-IDKUNDNR                     TO   RESP-IDKUNDNR-KEY          
037700     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
037800     END-IF                                                               
037900                                                                          
038000     IF WS-IDORDNR NUMERIC                                                
038100     MOVE WS-IDORDNR                      TO   RESP-IDORDNR-KEY           
038200     INSPECT RESP-IDORDNR-KEY  REPLACING LEADING ZERO BY SPACE            
038300     END-IF                                                               
038400                                                                          
038500     IF WS-IDKOLLI NUMERIC                                                
038600     MOVE WS-IDKOLLI                      TO   RESP-IDKOLLI-KEY           
038700     INSPECT RESP-IDKOLLI-KEY  REPLACING LEADING ZERO BY SPACE            
038800     END-IF                                                               
038900                                                                          
039000     IF WS-IDPRODNR NUMERIC                                               
039100     MOVE WS-IDPRODNR                     TO   RESP-IDPRODNR-KEY          
039200     INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE            
039300     END-IF                                                               
039400                                                                          
039500     MOVE REQU-L198-IDDC-KEY              TO   RESP-IDDC-KEY              
039600                                                                          
039700     .                                                                    
039800                                                                          
039900 B-GENERELL-KONTROLL  SECTION.                                            
040000     SKIP3                                                                
040100     IF WS-IDANSTNR NOT NUMERIC                                           
040200     OR WS-IDANSTNR =   ZERO                                              
040300         MOVE FEL                       TO   WS-INDATA-TEST               
040400         MOVE 'IDANSTNR'                TO  RESP-IDELMT-ERROR             
040500         MOVE FEL-INVALID-DATA          TO   RESP-IDMSG-ERROR             
040600     END-IF                                                               
040700     SKIP2                                                                
040800     IF WS-IDDISTR  NOT NUMERIC                                           
040900         MOVE FEL                       TO   WS-INDATA-TEST               
041000         MOVE 'IDDISTR'                 TO  RESP-IDELMT-ERROR             
041100         MOVE FEL-INVALID-DATA          TO   RESP-IDMSG-ERROR             
041200     ELSE                                                                 
041300         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
041400     END-IF                                                               
041500     SKIP2                                                                
041600     IF WS-IDKUNDNR NOT NUMERIC                                           
041700         MOVE FEL                       TO   WS-INDATA-TEST               
041800         MOVE 'IDKUNDNR'                TO  RESP-IDELMT-ERROR             
041900         MOVE FEL-INVALID-DATA          TO   RESP-IDMSG-ERROR             
042000     ELSE                                                                 
042100         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
042200     END-IF                                                               
042300     SKIP2                                                                
042400     IF WS-IDORDNR  NOT NUMERIC                                           
042500         MOVE FEL                       TO   WS-INDATA-TEST               
042600         MOVE 'IDORDNR'                 TO  RESP-IDELMT-ERROR             
042700         MOVE FEL-INVALID-DATA          TO   RESP-IDMSG-ERROR             
042800     ELSE                                                                 
042900         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
043000     END-IF                                                               
043100     SKIP2                                                                
043200     IF WS-IDPRODNR NOT NUMERIC                                           
043300         MOVE FEL                       TO   WS-INDATA-TEST               
043400         MOVE 'IDPRODNR'                TO  RESP-IDELMT-ERROR             
043500         MOVE FEL-INVALID-DATA          TO   RESP-IDMSG-ERROR             
043600     END-IF                                                               
043700                                                                          
043800                                                                          
043900     IF REQU-L198-IDDC-KEY IS > SPACE                                     
044000       CONTINUE                                                           
044100     ELSE                                                                 
044200       MOVE FEL                         TO WS-INDATA-TEST                 
044300         MOVE 'IDDC'                    TO  RESP-IDELMT-ERROR             
044400       MOVE FEL-INVALID-DATA            TO RESP-IDMSG-ERROR               
044500     END-IF                                                               
044600                                                                          
044700                                                                          
044800*                                                                         
044900     MOVE ZERO TO ANT-BILD-RADER                                          
045000     MOVE +1   TO INX                                                     
045100     PERFORM UNTIL INX > MAX-RAD-ANTAL                                    
045400         PERFORM BA-KONTROLLERA-RAD                                       
045500         ADD +1 TO INX                                                    
045600     END-PERFORM                                                          
046100     .                                                                    
046200     EJECT                                                                
046300 BA-KONTROLLERA-RAD     SECTION.                                          
046400     SKIP3                                                                
046410     MOVE INX                          TO RAD-INX                         
046420                                                                          
046500     IF REQU-L198-IDRADNR (INX) = ALL '+'                                 
047000*------------ OM MAN EJ VILL GODKÄNNA TOM BILD.                           
047100*        IF INX = +1                                                      
047200*            MOVE FEL                  TO WS-INDATA-TEST                  
047300*            MOVE FEL-743 (INDX)       TO MOD-TEMFSFEL                    
047400*            MOVE MFS-NUM-FAELT-FEL    TO MOD-IDRADNR-ATTR (INX)          
047500*        END-IF                                                           
047600         MOVE +501                     TO INX                             
047700     ELSE                                                                 
047800         MOVE INX TO ANT-BILD-RADER                                       
047900                     RESP-KVRADER                                         
048000         IF REQU-L198-KDARTURS (INX) = ALL '+'                            
048200             MOVE FEL                  TO WS-INDATA-TEST                  
048500             MOVE 'KDARTURS'           TO  RESP-IDELMT-ERROR              
048600             MOVE FEL-INVALID-DATA     TO  RESP-IDMSG-ERROR               
048700                     RESP-IDMSG-ERROR-LINE  (RAD-INX)                     
048800         ELSE                                                             
048900             INSPECT REQU-L198-IDRADNR (INX)                              
049000                     REPLACING LEADING SPACE                              
049100                                BY ZERO                                   
049200             IF REQU-L198-IDRADNR (INX) NUMERIC AND                       
049300                REQU-L198-IDRADNR (INX) > ZERO                            
049600                 CONTINUE                                                 
049700             ELSE                                                         
049800                 MOVE FEL                TO WS-INDATA-TEST                
050100                 MOVE 'IDRADNR'          TO  RESP-IDELMT-ERROR            
050200                 MOVE FEL-INVALID-DATA   TO   RESP-IDMSG-ERROR            
050300                      RESP-IDMSG-ERROR-LINE  (RAD-INX)                    
050400             END-IF                                                       
050401                                                                          
050410             MOVE REQU-L198-IDRADNR (INX) TO RESP-IDRADNR (INX)           
050500                                                                          
050600             INSPECT REQU-L198-KDARTURS (INX)                             
050700                     REPLACING LEADING ZERO                               
050800                                BY SPACE                                  
050900             IF REQU-L198-KDARTURS (INX) NUMERIC                          
051000               MOVE FEL                TO WS-INDATA-TEST                  
051400               MOVE 'KDARTURS'         TO  RESP-IDELMT-ERROR              
051500               MOVE FEL-INVALID-DATA   TO   RESP-IDMSG-ERROR              
051600                      RESP-IDMSG-ERROR-LINE  (RAD-INX)                    
051700             ELSE                                                         
051800               MOVE REQU-L198-KDARTURS (INX) TO ARTU-KDARTURS             
051900               MOVE WS-IDDISTR         TO ARTU-IDDISTR                    
052000               MOVE WS-IDDC            TO ARTU-IDDC                       
052100               CALL W400ARTU USING ARTU-W400ARTU                          
052200                                                                          
052300               IF ARTU-KDARTURS-NUM NUMERIC                               
052310                 CONTINUE                                                 
052400****              MOVE REQU-L198-KDARTURS (INX)                           
052500****                    TO RESP-KDARTURS (INX)                            
052800               ELSE                                                       
052900                 MOVE FEL                  TO WS-INDATA-TEST              
053300                 MOVE 'KDARTURS'           TO  RESP-IDELMT-ERROR          
053400                 MOVE FEL-INVALID-DATA     TO   RESP-IDMSG-ERROR          
053500                      RESP-IDMSG-ERROR-LINE  (RAD-INX)                    
053600               END-IF                                                     
053700             END-IF                                                       
053710             MOVE REQU-L198-KDARTURS (INX) TO RESP-KDARTURS (INX)         
053720                                                                          
053800         END-IF                                                           
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 C-RELATIONSKONTROLL  SECTION.                                            
054300                                                                          
054400     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
054500     IF  WS-INDATA-RATT                                                   
054600       PERFORM CB-KONTROLLERA-PACKARE                                     
054700       IF WS-INDATA-RATT                                                  
054800           PERFORM CC-BEHANDLA-RADER                                      
055000           MOVE ANT-BILD-RADER TO MAX-RAD-ANTAL                           
055100       ELSE                                                               
055300           PERFORM S04-REQU-RAD-TO-RESP                                   
055400       END-IF                                                             
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
055900                                                                          
056000     IF REQU-L198-IDPRODNR-KEY = ALL '+'                                  
056100         IF    REQU-L198-IDDISTR-KEY = ALL '+'                            
056200           AND REQU-L198-IDKUNDNR-KEY = ALL '+'                           
056300           AND REQU-L198-IDORDNR-KEY = ALL '+'                            
056400             IF WS-IDPRODNR > ZERO                                        
056500*                << ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT >>              
056600                 MOVE JA              TO  SOEK-VIA-PRODNR                 
057000                 MOVE ALL-PLUS        TO  RESP-IDDISTR-KEY                
057100                                          RESP-IDKUNDNR-KEY               
057200                                          RESP-IDORDNR-KEY                
057300             ELSE                                                         
057400                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
057600                 MOVE ALL-PLUS        TO  RESP-IDPRODNR-KEY               
057700             END-IF                                                       
057800         ELSE                                                             
057900             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
058100             MOVE ALL-PLUS            TO  RESP-IDPRODNR-KEY               
058200         END-IF                                                           
058300     ELSE                                                                 
058400         MOVE JA                       TO SOEK-VIA-PRODNR                 
058800         MOVE ALL-PLUS                 TO RESP-IDDISTR-KEY                
058900                                          RESP-IDKUNDNR-KEY               
059000                                          RESP-IDORDNR-KEY                
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
059500                                                                          
059600     MOVE 'N'                  TO WS-SLINGA-KLAR                          
059700*                                                                         
059800     MOVE WS-IDDISTR           TO W-4A1-IDDISTR                           
059900     MOVE WS-IDKUNDNR          TO W-4A1-IDKUNDNR                          
060000     MOVE WS-IDORDNR           TO W-4A1-IDORDNR                           
060100     PERFORM IMS-GU-KUNDORDER-SEK                                         
060200*                                                                         
060300     IF KUNDORDER-SEK-FINNS                                               
060400        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
060500                      SLINGA-KLAR                                         
060600        MOVE KORD-IDDISTR      TO W-401-IDDISTR                           
060700        MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                          
060800        MOVE KORD-IDORDNR5     TO W-401-IDORDNR                           
060900        MOVE KORD-IDPRODNR     TO W-401-IDPRODNR                          
061000        MOVE KORD-IDPRODNR     TO W-IDPRODNR-WDE6                         
061100        MOVE KORD-IDPLKLST     TO W-401-IDPLKLST                          
061200        PERFORM IMS-GU-KUNDORDER                                          
061300        MOVE KORD-KVORDRAD-LEVPL   TO WS-SPAR-KVORDRAD-LEVPL              
061400*                                                                         
061500        PERFORM IMS-GU-IDPRODNR                                           
061600        IF SEGMENT-FINNS                                                  
061700           IF WDE6-VORD-IDDC = WS-IDDC                                    
061800              MOVE 'J'                 TO   WS-SLINGA-KLAR                
061900              MOVE WDE6-VORD-IDPRODNR       TO   WS-IDPRODNR              
062000           ELSE                                                           
062200              MOVE FEL                   TO   WS-INDATA-TEST              
062300              MOVE FEL-ORDER-MISSING     TO   RESP-IDMSG-ERROR            
062400              MOVE 'IDORDNR'             TO   RESP-IDELMT-ERROR           
062401*             VI LÄGGER DETTA FELMEDD PÅ RADNIVÅ PGA PROBLEM I WEB        
062410*                     RESP-IDMSG-ERROR-LINE  (1)                          
062500           END-IF                                                         
062600        END-IF                                                            
062700        PERFORM IMS-GN-KUNDORDER-SEK                                      
062800        END-PERFORM                                                       
062900     END-IF                                                               
063000*                                                                         
063100     IF NOT SLINGA-KLAR                                                   
063300        MOVE FEL                         TO   WS-INDATA-TEST              
063400        MOVE FEL-ORDER-MISSING           TO   RESP-IDMSG-ERROR            
063500        MOVE 'IDORDNR'                   TO   RESP-IDELMT-ERROR           
063501*             VI LÄGGER DETTA FELMEDD PÅ RADNIVÅ PGA PROBLEM I WEB        
063510*                     RESP-IDMSG-ERROR-LINE  (1)                          
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 CB-KONTROLLERA-PACKARE SECTION.                                          
064000                                                                          
064100     MOVE 'N'                           TO WS-TRAEFF-PACKARE              
064200*                                                                         
064300     MOVE WS-IDDISTR                    TO W-4A1-IDDISTR                  
064400     MOVE WS-IDKUNDNR                   TO W-4A1-IDKUNDNR                 
064500     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
064600     PERFORM IMS-GU-KUNDORDER-SEK                                         
064700*                                                                         
064800     IF KUNDORDER-SEK-FINNS                                               
064900        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
065000                      WS-TRAEFF-PACKARE = 'J'                             
065100        MOVE KORD-IDDISTR              TO W-401-IDDISTR                   
065200        MOVE KORD-IDKUNDNR             TO W-401-IDKUNDNR                  
065300        MOVE KORD-IDORDNR5             TO W-401-IDORDNR                   
065400        MOVE KORD-IDPRODNR             TO W-401-IDPRODNR                  
065500        MOVE KORD-IDPLKLST             TO W-401-IDPLKLST                  
065600        PERFORM IMS-GU-KUNDORDER                                          
065700*                                                                         
065800        MOVE KORD-IDPRODNR             TO WS-JFR-IDPRODNR                 
065900        MOVE KORD-IDUSER               TO WS-JFR-IDANSTNR                 
066100        IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                              
066200           WS-IDANSTNR = WS-JFR-IDANSTNR-5                                
066300           MOVE 'J'                    TO WS-TRAEFF-PACKARE               
066400           IF KORD-KDPAKOLL NOT = ZERO                                    
066500*----------------------------------------------AVVIKELSEKONTROLL          
066600*----------------------------------------------PÅGÅR                      
066700              MOVE FEL                 TO WS-INDATA-TEST                  
066900              MOVE FEL-DEV-IN-PROGRESS TO RESP-IDMSG-ERROR                
066910*             VI LÄGGER DETTA FELMEDD PÅ RADNIVÅ PGA PROBLEM I WEB        
066920*                     RESP-IDMSG-ERROR-LINE  (1)                          
067000           END-IF                                                         
067100        END-IF                                                            
067200*                                                                         
067300        PERFORM IMS-GN-KUNDORDER-SEK                                      
067400        END-PERFORM                                                       
067500     ELSE                                                                 
067600        MOVE FEL                         TO   WS-INDATA-TEST              
067800        MOVE FEL-ORDER-MISSING           TO   RESP-IDMSG-ERROR            
067900        MOVE 'IDORDNR'                   TO   RESP-IDELMT-ERROR           
068000     END-IF                                                               
068100*                                                                         
068200     IF WS-TRAEFF-PACKARE = 'N'                                           
068400*----------------------------------------------ANGIVEN PACKARE            
068500*----------------------------------------------SAKNAS PÅ ORDERN           
068900        MOVE ALL-PLUS            TO RESP-IDDISTR-KEY                      
069000                                    RESP-IDKUNDNR-KEY                     
069100                                    RESP-IDORDNR-KEY                      
069200        MOVE FEL                 TO WS-INDATA-TEST                        
069400        MOVE FEL-WRONG-PICKER    TO RESP-IDMSG-ERROR                      
069410*             VI LÄGGER DETTA FELMEDD PÅ RADNIVÅ PGA PROBLEM I WEB        
069420*                     RESP-IDMSG-ERROR-LINE  (1)                          
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 CC-BEHANDLA-RADER       SECTION.                                         
069900                                                                          
070000     IF ANT-BILD-RADER > ZERO                                             
070100        MOVE +1 TO INX                                                    
070200        PERFORM UNTIL INX > ANT-BILD-RADER                                
070300*                                                                         
070400        MOVE REQU-L198-IDRADNR (INX) TO WS-IDRADNR                        
070500        MOVE 'N'               TO WS-TRAEFF-RAD                           
070600*                                                                         
070700        MOVE WS-IDDISTR        TO W-4A1-IDDISTR                           
070800        MOVE WS-IDKUNDNR       TO W-4A1-IDKUNDNR                          
070900        MOVE WS-IDORDNR        TO W-4A1-IDORDNR                           
071000        PERFORM IMS-GU-KUNDORDER-SEK                                      
071100*                                                                         
071200        IF KUNDORDER-SEK-FINNS                                            
071300           PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                          
071400                         WS-TRAEFF-RAD = 'J'                              
071500           MOVE KORD-IDDISTR   TO W-401-IDDISTR                           
071600           MOVE KORD-IDKUNDNR  TO W-401-IDKUNDNR                          
071700           MOVE KORD-IDORDNR5  TO W-401-IDORDNR                           
071800           MOVE KORD-IDPRODNR  TO W-401-IDPRODNR                          
071900           MOVE KORD-IDPLKLST  TO W-401-IDPLKLST                          
072000           PERFORM IMS-GU-KUNDORDER                                       
072100*                                                                         
072200           MOVE KORD-IDPRODNR  TO WS-JFR-IDPRODNR                         
072300           MOVE KORD-IDUSER    TO WS-JFR-IDANSTNR                         
072400           IF WS-IDPRODNR = WS-JFR-IDPRODNR                               
072500              PERFORM CCA-KONTROLLERA-RADER                               
072600           END-IF                                                         
072700*                                                                         
072800           PERFORM IMS-GN-KUNDORDER-SEK                                   
072900           END-PERFORM                                                    
073000           IF WS-TRAEFF-RAD = 'N'                                         
073100              MOVE FEL               TO WS-INDATA-TEST                    
073200              MOVE FEL-LINE-NOT-BELONG-TO-PICK                            
073300                                     TO RESP-IDMSG-ERROR                  
073500                      RESP-IDMSG-ERROR-LINE  (INX)                        
073600           END-IF                                                         
073700        END-IF                                                            
073800        ADD +1 TO INX                                                     
073900        END-PERFORM                                                       
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300 CCA-KONTROLLERA-RADER SECTION.                                           
074400                                                                          
074500     MOVE WS-IDRADNR                 TO W-420-IDPURAD2                    
074600*                                                                         
074700     IF WS-IDANSTNR = WS-JFR-IDANSTNR-5                                   
074800        PERFORM IMS-GU-RAD                                                
074900*                                                                         
075000        IF SEGMENT-FINNS                                                  
075100           MOVE 'J'               TO WS-TRAEFF-RAD                        
075200        END-IF                                                            
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600 D-BEHANDLA-RAD       SECTION.                                            
075700                                                                          
075800     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR                       
075900     MOVE REQU-L198-IDRADNR (INX) TO W-420-IDPURAD                        
076000     MOVE REQU-L198-KDARTURS (INX) TO WS-KDARTURS                         
076100     SKIP2                                                                
076200     PERFORM IMS-GHU-RAD-SEK                                              
076300     SKIP2                                                                
076400     MOVE WS-KDARTURS            TO  ORAD-KDARTURS                        
076500     SKIP2                                                                
076600     PERFORM IMS-REPL-RAD                                                 
076700     IF SEGMENT-FINNS                                                     
076800       MOVE JA                   TO  VISA-UPPDATERAT-MED-SW               
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 H-AVSLUT             SECTION.                                            
077300                                                                          
080400     IF VISA-UPPDATERAT-MED                                               
080500       MOVE UPDATE-DONE                  TO RESP-IDMSG-INFO               
080700     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600*    --- DISPATCHER SECTIONS                                              
083700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
083800                                                                          
083900     MOVE 'GETARG'               TO SUB-KDFUNC                            
084000     MOVE 'CARPARTS.LDC.UPDATEPARTORIGIN'    TO SUB-ADDISPABS             
084100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
084200                                                                          
084300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
084400                                                                          
084500     IF SUB-KDRC > 0                                                      
084600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
084700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
084800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
084900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
085000     END-IF                                                               
085100     .                                                                    
085200     SKIP3                                                                
085300 S02-RETURN-RESPONSE SECTION.                                             
085400                                                                          
085500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
085600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
085700                                                                          
085800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
085900                                                                          
086000     IF SUB-KDRC > 0                                                      
086100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
086200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
086300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
086400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800     SKIP3                                                                
086900                                                                          
088600     EJECT                                                                
088700 S04-REQU-RAD-TO-RESP   SECTION.                                          
088800     SKIP3                                                                
088900     MOVE +1 TO RAD-INX                                                   
089000     PERFORM UNTIL RAD-INX > MAX-RAD-ANTAL                                
089100         MOVE REQU-L198-IDRADNR (RAD-INX)                                 
089200                                  TO RESP-IDRADNR (RAD-INX)               
089300         MOVE REQU-L198-KDARTURS (RAD-INX)                                
089400                                  TO RESP-KDARTURS (RAD-INX)              
089500                                                                          
089900         ADD +1 TO RAD-INX                                                
090000     END-PERFORM                                                          
090100     .                                                                    
090200     EJECT                                                                
090300* IMS SEKTIONER                                                           
090400     SKIP3                                                                
090500     EJECT                                                                
090600 IMS-GU-KUNDORDER SECTION.                                                
090700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
090800            DELIMITED BY SIZE INTO SSA1                                   
090900     MOVE '  GE' TO GODK-STATUSKODER                                      
091000     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1                  
091100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
091200     PERFORM IMS-STATUSKONTROLL                                           
091300     SKIP3                                                                
091400     .                                                                    
091500 IMS-GU-RAD       SECTION.                                                
091600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
091700            DELIMITED BY SIZE INTO SSA1                                   
091800     STRING 'WDE411  (IDPURAD  =' W-WDE420-KEYSEQ-X ')'                   
091900            DELIMITED BY SIZE INTO SSA2                                   
092000     MOVE '  GE' TO GODK-STATUSKODER                                      
092100     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1 SSA2             
092200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     SKIP3                                                                
092500     .                                                                    
092600 IMS-GU-IDPRODNR   SECTION.                                               
092700     STRING 'WDE601  (IDPRODNR =' W-WDE601KY-X ')'                        
092800            DELIMITED BY SIZE INTO SSA1                                   
092900     MOVE '  GE' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-WDE601 SSA1                   
093100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     EJECT                                                                
093400     .                                                                    
093500 IMS-GU-KUNDORDER-SEK SECTION.                                            
093600     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
093700            DELIMITED BY SIZE INTO SSA1                                   
093800     MOVE '  GE' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GU    WDE4A-PCB DLI-IO-AREA SSA1                  
094000     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
094100                               STATUS-KUNDORDER-SEK-WS                    
094200     PERFORM IMS-STATUSKONTROLL                                           
094300     SKIP3                                                                
094400     .                                                                    
094500 IMS-GN-KUNDORDER-SEK SECTION.                                            
094600     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
094700            DELIMITED BY SIZE INTO SSA1                                   
094800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094900     CALL CBLTDLI USING GN    WDE4A-PCB DLI-IO-AREA SSA1                  
095000     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
095100                               STATUS-KUNDORDER-SEK-WS                    
095200     PERFORM IMS-STATUSKONTROLL                                           
095300     .                                                                    
095400     EJECT                                                                
095500 IMS-GHU-RAD-SEK  SECTION.                                                
095600     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
095700            DELIMITED BY SIZE INTO SSA1                                   
095800     MOVE '    ' TO GODK-STATUSKODER                                      
095900     CALL CBLTDLI USING GHU    WDE4B-PCB DLI-IO-AREA SSA1                 
096000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
096100     PERFORM IMS-STATUSKONTROLL                                           
096200     SKIP3                                                                
096300     .                                                                    
096400 IMS-REPL-RAD           SECTION.                                          
096500     MOVE '    ' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING REPL WDE4B-PCB DLI-IO-AREA                        
096700     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
096800     PERFORM IMS-STATUSKONTROLL                                           
096900     .                                                                    
097000     EJECT                                                                
097100 IMS-STATUSKONTROLL SECTION.                                              
097200     SET STATUS-IX TO 1                                                   
097300     SEARCH GODK-STATUS AT END CALL FELLOG                                
097400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
097500     END-SEARCH                                                           
097600     .                                                                    
