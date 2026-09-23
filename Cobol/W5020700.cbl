000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5020700.                                                
000301 AUTHOR.         RAHUL JAIN.                                              
000401 DATE-WRITTEN.   DEC-2012.                                                
000501                                                                          
000601*    REMARKS.                                                             
000701*                                                                         
000801*    FUNKTION.                                                            
000901*        IMSDC UPPDATERINGSPROGRAM FÖR EKONOMI.                           
001001*                                                                         
001101*        PROGRAMMET LÄSER WDK7 OCH PLOCKAR BORT                           
001201*        BEFINTLIGA BESTÄLLNINGSPRISER  PÅ WDK724.                        
001301*                                                                         
001401*    INDATA.                                                              
001501*        TRANSAKTION: W5T207                                              
001601*                     W5T207U                                             
001701*                                                                         
001801*        MID:         W5I20701                                            
001901*                                                                         
002001*    UTDATA.                                                              
002101*        MOD:         W5O20701                                            
002201*                                                                         
002301     EJECT                                                                
002401 ENVIRONMENT DIVISION.                                                    
002501 DATA DIVISION.                                                           
002601 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002801*    -COPY WY2000W1                                                       
002901     SKIP3                                                                
003001 77  JA                      PIC X          VALUE 'J'.                    
003101 77  NEJ                     PIC X          VALUE 'N'.                    
003201                                                                          
003301 77  NYCKLAR-OK              PIC X          VALUE 'J'.                    
003401 77  FLFEL-FAELT             PIC X          VALUE 'N'.                    
003501 77  FLSLUTA-LAS             PIC X          VALUE 'N'.                    
003502 77  W-DATE-AAMM             PIC 9(4)       VALUE ZERO.                   
003601                                                                          
003701 77  INDX                    PIC S9(9)      VALUE +0   COMP SYNC.         
003801                                                                          
003901 01  SUBPGM.                                                              
004001     03  CBLTDLI             PIC X(8)       VALUE 'CBLTDLI '.             
004101     03  FELLOG              PIC X(8)       VALUE 'FELLOG  '.             
004201     03  WDATKONV            PIC X(8)       VALUE 'WDATKONV'.             
004301     03  W005INIT            PIC X(8)       VALUE 'W005INIT'.             
004401     03  W510PRTR            PIC X(8)       VALUE 'W510PRTR'.             
004402     03  W510CURR            PIC X(8)       VALUE 'W510CURR'.             
004501     03  W006KOM             PIC X(8)       VALUE 'W006KOM '.             
004700                                                                          
004800 01  W-IDTRANS               PIC X(4)       VALUE SPACE.                  
004900     88 EGEN-TRANS                          VALUE '5207'.                 
005000     88 GODK-TRANS                          VALUE '5206' '5207'.          
005100                                                                          
005200*01  -COPY WWDC99                                                         
005300*                                                                         
005400*01  -COPY WWIDFTG                                                        
005500                                                                          
005600 01  DAGENS-DAT.                                                          
005700     03  DAGENS-SEKEL        PIC 9(2).                                    
005800     03  DAGENS-DATUM        PIC 9(6).                                    
005900     03  FILLER  REDEFINES DAGENS-DATUM.                                  
006000      05 DAGENS-AAR          PIC 9(2).                                    
006100      05 FILLER              PIC 9(4).                                    
006200                                                                          
006300 01  DAGENS-TID              PIC 9(8).                                    
006400 01  FILLER  REDEFINES DAGENS-TID.                                        
006500     03  DAGENS-KLOCK        PIC 9(6).                                    
006600     03  FILLER              PIC 9(2).                                    
006700                                                                          
006801 01  DIVERSE.                                                             
006901     03  WS-KDVALISO         PIC X(3)       VALUE SPACE.                  
007001     03  W-PRKURS            PIC S9(5)V9(5) VALUE +0   COMP-3.            
007101     03  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.            
007201     03  W-RETULF            PIC S9(3)V9(4) VALUE +0.                     
007301     03  DAGENS-AAMMDD       PIC 9(6).                                    
007401     03  W-PRARTBEL          PIC S9(8)V9(5) VALUE +0.                     
007501     03  WS-PRARTBES-PR      PIC S9(7)V9(2) VALUE +0   COMP-3.            
007601                                                                          
008200 01  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
008301                                                                          
008401 01  WS-TIPRLIST-6           PIC  9(6).                                   
008500                                                                          
008600 01  DATUM-FAELT.                                                         
008700     03  W-TIPRLIST          PIC 9(6).                                    
008800     03  W-DAPRLIST-MAX      PIC 9(8)    VALUE 99999999.                  
008900     03  W-DAPRLIST          PIC 9(8).                                    
009000     03  W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
009100      05 FILLER              PIC 9(2).                                    
009200      05 W-LISTDATUM         PIC 9(6).                                    
009300                                                                          
009401 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
009501 01  P-TO-P-AREA.                                                         
009601     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
009701     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
009801     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
009901     03  P-TO-P-TRANSKOD         PIC  X(7).                               
010001     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
010101     03  P-TO-P-FROM-MID         PIC  X(4).                               
010201     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
010301     03  P-TO-P-DATA             PIC  X(1000).                            
010401     EJECT                                                                
010501                                                                          
010600 01  MAX-MOD-LAENGD          PIC S9(4)   VALUE +411  COMP SYNC.           
010700     EJECT                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
011100     03  W-IDDC-X.                                                        
011200         05  W-IDDC          PIC X(02)   VALUE SPACE.                     
011300     03  W-KDSEGKEY-X.                                                    
011400         05  W-KDSEGKEY      PIC X(1)    VALUE SPACE.                     
011500     03  W-IDSKYLT-X.                                                     
011600         05    W-IDSKYLT     PIC X(3)    VALUE 'GB '.                     
011701     03  W-IDDC-B6-X.                                                     
011801         05 W-IDDC-B6        PIC X(2).                                    
011901     03  W-IDLEVNR-X.                                                     
012001         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
012101     03  W-IDLAND-X.                                                      
012201         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
013201     SKIP3                                                                
013301 01  MEDDELANDE.                                                          
013401     03  W-FEL-1               PIC X(26)   VALUE                          
013501             'PART NOT FOUND            '.                                
013601                                                                          
013701     03  W-FEL-2               PIC X(26)   VALUE                          
013801             'PART EXPIRED              '.                                
013901                                                                          
014001     03  W-FEL-3               PIC X(26)   VALUE                          
014101             'HIGHLIGHTED FIELDS WRONG  '.                                
014201                                                                          
014301     03  W-FEL-4               PIC X(26)   VALUE                          
014401             'PART NUMBER NOT NUMERIC   '.                                
014501                                                                          
014601     03  W-FEL-5               PIC X(40)   VALUE                          
014701             'PRESS PF-KEY FOR UPDATE              '.                     
014801                                                                          
014901     03  W-FEL-6               PIC X(40)   VALUE                          
015001             'GIVE VALUES WHEN UPDATE              '.                     
015101                                                                          
015201     03  W-MED-1               PIC X(26)   VALUE                          
015301             'UPDATE DONE               '.                                
015401                                                                          
015501 01  MESSAGE-CODES.                                                       
015601     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015701     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015801     EJECT                                                                
016401 01  WDECEDIT                  PIC X(8)    VALUE 'WDECEDIT'.              
016501     SKIP3                                                                
016601*01       -COPY WDECAREA                                                  
016701     EJECT                                                                
016801*                   ****    PARAMETRAR TILL W005INIT                      
016901*01  -COPY WMSGINIT                                                       
017001     EJECT                                                                
017101                                                                          
017201 01  FILLER                    PIC X(8)    VALUE 'W510PRTR'.              
017301*01       -COPY W510PRTR                                                  
017401     EJECT                                                                
017402 01  FILLER                    PIC X(8)    VALUE 'W510CURR'.              
017403*01       -COPY W510CURR                                                  
017404     EJECT                                                                
017501                                                                          
017601 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
017701     SKIP3                                                                
017801*01       -COPY WDATAREA                                                  
017901     EJECT                                                                
018001 01  FILLER                    PIC X(16) VALUE 'WDH801-AREA   '.          
018101*01  WDH801    -COPY WDH801                                               
018201     EJECT                                                                
018301******************************************************************        
018401*                                                                         
018501*                AREOR FOR MFS OCH SKÄRMHANTERING                         
018601*                                                                         
018701******************************************************************        
018801 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
018901     SKIP3                                                                
019001*01  MID  -COPY W5I20701                                                  
019101     EJECT                                                                
019201*01       -COPY WMSGAREA                                                  
019301     EJECT                                                                
019401*  03 MOD -COPY W5O20701   -PRE MOD- -RED MSG-AREA                        
019501     EJECT                                                                
019601*01       -COPY WMFSAREA                                                  
019701     EJECT                                                                
019801******************************************************************        
019901*                                                                         
020001*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020101*                                                                         
020201******************************************************************        
020301                                                                          
020401 01  IMS-WS.                                                              
020501     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
020601     SKIP3                                                                
020701*                        **** STATUS-KOD FRÅN IMS ****                    
020801     03    STATUS-WS           PIC XX.                                    
020901         88    SEGMENT-FINNS               VALUE '  '.                    
021001         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
021101     SKIP3                                                                
021201     03    GODK-STATUSKODER.                                              
021301         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
021401     SKIP3                                                                
021501 01  SSA1                      PIC X(64).                                 
021601 01  SSA2                      PIC X(64).                                 
021701 01  SSA3                      PIC X(64).                                 
021801     EJECT                                                                
021901*                        **** IMS FUNKTIONSKODER ****                     
022001*01       -COPY W0003                                                     
022101     EJECT                                                                
022201*                                DLI INPUT-OUTPUT AREA                    
022301*01  WDK601 -COPY WDK601                                                  
022401     EJECT                                                                
022501*01  WDK711 -COPY WDK711                                                  
022601     EJECT                                                                
022701*01  WDK724 -COPY WDK724                                                  
022801     EJECT                                                                
022901*01  WDD311 -COPY WDD311 -PRE WDD3-                                       
023001     EJECT                                                                
023101                                                                          
023201*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
023301 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
023401 01  KOM-IO-AREA.                                                         
023501*  03     -COPY WMSGKOM                                                   
023601*                                                                         
023701 01  FILLER                 PIC X(16)   VALUE 'KOM-IO-AREA2'.             
023801 01  KOM-IO-AREA2.                                                        
023901*  03      -COPY W5I11201 -PRE 5112-                                      
024001     EJECT                                                                
024101                                                                          
024201 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
024301*01  WLLEVA01 -COPY WDF101                                                
024401     EJECT                                                                
024501                                                                          
024601 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
024701*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
024801     EJECT                                                                
024901                                                                          
025001 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
025101 01   DLI-IO-AREA-B601.                                                   
025201*     03  -COPY WDB601                                                    
025301     EJECT                                                                
025401                                                                          
025901 LINKAGE SECTION.                                                         
026001*01       -COPY W0009     -PRE MSG-                                       
026101     EJECT                                                                
026201*01       -COPY W0009     -PRE ALT-                                       
026301     EJECT                                                                
026401*01       -COPY W0008     -PRE USEA-                                      
026501         05  FILLER           PIC X.                                      
026601     EJECT                                                                
026701*01       -COPY W0008     -PRE WDK6-                                      
026801     05  FILLER                  PIC X(11).                               
026901     EJECT                                                                
027001*01       -COPY W0008     -PRE WDK7-                                      
027101     05  FILLER                  PIC X(11).                               
027201     EJECT                                                                
027301*01       -COPY W0008     -PRE WDD3-                                      
027401     05  FILLER                  PIC X(8).                                
027501     EJECT                                                                
027601*01       -COPY W0008     -PRE WDH8-                                      
027701      05 FILLER                  PIC X(18).                               
027801     EJECT                                                                
027901*01       -COPY W0008     -PRE WDK62-                                     
028001     05 FILLER                   PIC X(13).                               
028101     EJECT                                                                
028201*01       -COPY W0008     -PRE KOMA-                                      
028301     05  FILLER                  PIC X.                                   
028401     EJECT                                                                
028501*01  -COPY W0008     -PRE LEV-                                            
028601     05  FILLER                  PIC X(5).                                
028701     EJECT                                                                
028801*01  -COPY W0008     -PRE WDB6-                                           
028901     05 FILLER                   PIC X(18).                               
029001     EJECT                                                                
029101*01  -COPY W0008  -PRE 9305-                                              
029201     05  FILLER                  PIC X.                                   
029301                                                                          
029401 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
029501                           WDK6-PCB WDK7-PCB WDD3-PCB WDH8-PCB            
029601                           WDK62-PCB KOMA-PCB                             
029701                           LEV-PCB WDB6-PCB 9305-PCB.                     
029801 MAIN SECTION.                                                            
029901     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
030001                           WDK6-PCB WDK7-PCB WDD3-PCB WDH8-PCB            
030101                           WDK62-PCB KOMA-PCB                             
030201                           LEV-PCB WDB6-PCB 9305-PCB.                     
030301     SKIP3                                                                
030401     PERFORM IMS-GET-MSG                                                  
030501                                                                          
030601     IF SEGMENT-FINNS                                                     
030701        PERFORM A-INIT                                                    
030801        PERFORM B-KOLLA-NYCKEL                                            
030901        IF MFS-UPD-X                                                      
031001          PERFORM IMS-GN-MSG-KOM                                          
031101        END-IF                                                            
031201        IF NYCKLAR-OK = JA                                                
031301           IF MFS-UPDATE                                                  
031401           OR MFS-UPD-X                                                   
031501* **          HÄR BÖRJAR UPPDATERING                **                    
031601              IF MID-TIPRLIST-U NUMERIC                                   
031701                 PERFORM C-KONTROLLERA-MID-MOT-BAS                        
031801              ELSE                                                        
031901                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR            
032001                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
032101                 MOVE JA                TO FLFEL-FAELT                    
032201              END-IF                                                      
032301              IF MID-IDLEVNR = ALL '+'                                    
032401                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDLEVNR-ATTR               
032501                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
032601                 MOVE JA                TO FLFEL-FAELT                    
032701              END-IF                                                      
032801                                                                          
032901              PERFORM MFS-ROER-EJ-FAELT-UTDATA                            
033001                                                                          
033101              IF FLFEL-FAELT = JA                                         
033201                 MOVE MFS-ROER-EJ-FAELT  TO  MOD-TIPRLIST-U               
033301                 MOVE MFS-ROER-EJ-FAELT  TO  MOD-IDLEVNR                  
033401              ELSE                                                        
033501                 MOVE MFS-RENSA-FAELT    TO MOD-TIPRLIST-U                
033601                 MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR                   
033701                 MOVE MFS-FORMATETS-ATTR TO MOD-TIPRLIST-U-ATTR           
033801                 MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-ATTR              
033901                 PERFORM D-UPPDATERA                                      
034001                 PERFORM S-BEHANDLA-BESTPRIS-INFO                         
034101                 MOVE W-MED-1            TO MOD-TEMFSINF                  
034201              END-IF                                                      
034301                                                                          
034401* **          HÄR SLUTAR UPPDATERING                 **                   
034501     EJECT                                                                
034601           ELSE                                                           
034701* **          HÄR BÖRJAR SÖKNING                     **                   
034801              IF MID-IDARTNR-IN = ALL '+' AND                             
034901                 MID-TIPRLIST-U NOT = ALL '+' AND EGEN-TRANS              
035001                 MOVE W-FEL-5 TO MOD-TEMFSFEL                             
035101                 PERFORM MFS-ROER-EJ-FAELT-UTDATA                         
035201                 MOVE MFS-ROER-EJ-FAELT TO  MOD-TIPRLIST-U                
035301                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
035401                      MOD-TIPRLIST-U-ATTR                                 
035501                      MOD-IDLEVNR-ATTR                                    
035601              ELSE                                                        
035701                 PERFORM IMS-GU-WDK601                                    
035801                                                                          
035901                 IF SEGMENT-FINNS                                         
036001                    IF ART-KDERS-UTG = 0                                  
036101                       PERFORM IMS-GHU-WDK711                             
036201                       IF SEGMENT-FINNS                                   
036301                         PERFORM S-BEHANDLA-BESTPRIS-INFO                 
036401                         PERFORM F-BEHANDLA-BENAMNING                     
036501                         MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U           
036601                         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR              
036701                       ELSE                                               
036801                         MOVE W-FEL-1 TO MOD-TEMFSFEL                     
036901                         PERFORM MFS-RENSA-FAELT-UTDATA                   
037001                         MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U           
037101                         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR              
037201                       END-IF                                             
037301                    ELSE                                                  
037401                       MOVE W-FEL-2 TO MOD-TEMFSFEL                       
037501                       PERFORM MFS-RENSA-FAELT-UTDATA                     
037601                       MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U             
037701                       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                
037801                    END-IF                                                
037901                 ELSE                                                     
038001                    MOVE W-FEL-1 TO MOD-TEMFSFEL                          
038101                    PERFORM MFS-RENSA-FAELT-UTDATA                        
038201                    MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                
038301                    MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                   
038401                 END-IF                                                   
038501                                                                          
038601              END-IF                                                      
038701* **          HÄR SLUTAR SÖKNING                    **                    
038801                                                                          
038901           END-IF                                                         
039001     EJECT                                                                
039101        ELSE                                                              
039201           MOVE W-FEL-4 TO MOD-TEMFSFEL                                   
039301           PERFORM MFS-RENSA-FAELT-UTDATA                                 
039401           MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                         
039501           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                            
039601        END-IF                                                            
039701        IF MFS-UPD-X                                                      
039801          IF MSG-KOM-IDMFSMED = SPACE                                     
039901            MOVE INF-UPDATE-DONE TO MSG-KOM-IDMFSMED                      
040001          END-IF                                                          
040101          MOVE SPACE             TO MSG-KOM-KDSVAR                        
040201          PERFORM IMS-INSERT-MSG-KOM                                      
040301        ELSE                                                              
040401          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
040501          PERFORM IMS-INSERT-MSG                                          
040601        END-IF                                                            
040701     END-IF                                                               
040801                                                                          
040901     MOVE ZERO TO RETURN-CODE                                             
041001     GOBACK                                                               
041101     .                                                                    
041201     EJECT                                                                
041301 A-INIT SECTION.                                                          
041401                                                                          
041501     ACCEPT DAGENS-DATUM FROM DATE                                        
041601     IF DAGENS-AAR < 50                                                   
041701       MOVE 20     TO DAGENS-SEKEL                                        
041801     ELSE                                                                 
041901       MOVE 19     TO DAGENS-SEKEL                                        
042001     END-IF                                                               
042101     ACCEPT DAGENS-TID    FROM TIME                                       
042201                                                                          
042301     MOVE FUNCTION CURRENT-DATE (3:4) TO W-DATE-AAMM                      
042501     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
042601                                                                          
042701     MOVE 'J'      TO  JA                                                 
042801     MOVE 'N'      TO  NEJ                                                
042901                                                                          
043001     MOVE 'J'      TO  NYCKLAR-OK                                         
043101     MOVE 'N'      TO  FLFEL-FAELT                                        
043201     MOVE 'N'      TO  FLSLUTA-LAS                                        
043301                                                                          
043401     MOVE +0       TO  INDX                                               
043501                                                                          
043601     MOVE SPACE    TO  W-IDTRANS                                          
043701                       IDARTNR-WS                                         
043801                                                                          
043901     MOVE +693     TO  MAX-MOD-LAENGD                                     
044001                                                                          
044101     MOVE +0       TO  W-IDARTNR                                          
044201                                                                          
044301     IF MSG-DUBBLA-TRANSKODER                                             
044401        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I20701                
044501        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                          
044601        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
044701     ELSE                                                                 
044801        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I20701                 
044901        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                          
045001        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
045101     END-IF                                                               
045201                                                                          
045301     MOVE MSG-KDTRTYP             TO MFS-KDTRTYP                          
045401     MOVE MFS-IDTRANS             TO W-IDTRANS                            
045501                                                                          
045601     MOVE LOW-VALUE       TO MSG-AREA                                     
045701     MOVE 'W5O207N1'      TO MFS-IDMOD                                    
045801     MOVE '5207'          TO MOD-IDTRANS                                  
045901                                                                          
046001     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
046101                             MOD-TEMFSFEL                                 
046201                             MOD-TEMFSINF                                 
046301     .                                                                    
046401     EJECT                                                                
046501 B-KOLLA-NYCKEL SECTION.                                                  
046601     SKIP3                                                                
046701     MOVE ALL '+' TO MSGI-WMSGINIT                                        
046801     MOVE '001'             TO MSGI-KDCALL                                
046901     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
047001     MOVE '5207'            TO MSGI-IDTRANS                               
047101     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
047201     IF MFS-IDTRANS = '5207'                                              
047301     OR (MID-IDARTNR-IN NUMERIC                                           
047401     AND MID-IDARTNR-IN > ZERO)                                           
047501         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
047601     END-IF                                                               
047701     IF MFS-IDTRANS = '5207'                                              
047801       IF MID-IDDC-IN NOT = SPACE                                         
047901          MOVE MID-IDDC-IN  TO MSGI-IDDC-KEY                              
048001       ELSE                                                               
048101          MOVE MID-IDDC-UT  TO MSGI-IDDC-KEY                              
048201       END-IF                                                             
048301     END-IF                                                               
048401     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048501                                                                          
048601     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
048701     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
048801                                                                          
048901     IF MID-IDARTNR-IN = ALL '+'                                          
049001        CONTINUE                                                          
049101     ELSE                                                                 
049201        MOVE ' '            TO MFS-KDTRTYP                                
049301     END-IF                                                               
049401                                                                          
049501     IF NOT EGEN-TRANS                                                    
049601        MOVE ' '            TO MFS-KDTRTYP                                
049701     END-IF                                                               
049801                                                                          
049901     IF IDARTNR-WS NOT NUMERIC                                            
050001        MOVE NEJ            TO NYCKLAR-OK                                 
050101     ELSE                                                                 
050201        MOVE IDARTNR-WS     TO W-IDARTNR                                  
050301     END-IF                                                               
050401                                                                          
050501     MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                                
050601                               WS-IDDC                                    
050701                                                                          
050801     IF GOOD-DC                                                           
050901       MOVE WS-IDDC         TO W-IDDC                                     
051001     ELSE                                                                 
051101       MOVE NEJ             TO NYCKLAR-OK                                 
051201     END-IF                                                               
051301                                                                          
051401*    -- KONTROLL AV IDFTG                                                 
051501     IF  MSGI-IDFTG NUMERIC                                               
051601     AND MSGI-IDFTG   > ZERO                                              
051701       MOVE MSGI-IDFTG      TO WS-IDFTG                                   
051801                               MOD-IDFTG-UT                               
051901       IF NDC-CN OR LDC-CN                                                
052001         IF IDFTG-CN                                                      
052101           CONTINUE                                                       
052201         ELSE                                                             
052301           MOVE W-FEL-6     TO MOD-TEMFSFEL                               
052401           MOVE JA          TO FLFEL-FAELT                                
052501         END-IF                                                           
052601       ELSE                                                               
052701         IF NDC-NA                                                        
052801           IF IDFTG-US                                                    
052901             CONTINUE                                                     
053001           ELSE                                                           
053101             MOVE W-FEL-6   TO MOD-TEMFSFEL                               
053201             MOVE JA        TO FLFEL-FAELT                                
053301           END-IF                                                         
053401         END-IF                                                           
053501       END-IF                                                             
053601     ELSE                                                                 
053701       MOVE W-FEL-6         TO MOD-TEMFSFEL                               
053801       MOVE JA              TO FLFEL-FAELT                                
053901     END-IF                                                               
054001                                                                          
054101     IF NYCKLAR-OK = NEJ                                                  
054201        IF GODK-TRANS                                                     
054301           MOVE IDARTNR-WS TO MOD-IDARTNR-UT                              
054401           INSPECT MOD-IDARTNR-UT REPLACING                               
054501                   LEADING ZERO BY SPACE                                  
054601        ELSE                                                              
054701           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
054801        END-IF                                                            
054901     ELSE                                                                 
055001        MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                 
055101        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
055201     END-IF                                                               
055301     .                                                                    
055401     EJECT                                                                
055501 C-KONTROLLERA-MID-MOT-BAS SECTION.                                       
055601     SKIP3                                                                
055701     PERFORM IMS-GHU-WDK711                                               
055801                                                                          
055901     IF SEGMENT-FINNS                                                     
056001                                                                          
056101        IF MID-TIPRLIST-U = ALL '+' OR MID-IDLEVNR = ALL '+'              
056201           MOVE W-FEL-6 TO MOD-TEMFSFEL                                   
056301           MOVE JA TO FLFEL-FAELT                                         
056401        ELSE                                                              
056501                                                                          
056601* **       HÄR LÄSES BEST.PRIS SEGMENT FÖR ATT               **           
056701* **       KONTROLLERA OM PRIS FÖR ANGIVET                   **           
056801* **       DATUM FINNS LAGRAT OCH KAN TAS BORT               **           
056901                                                                          
057001           MOVE 1 TO INDX                                                 
057101           PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                    
057201                         FLSLUTA-LAS = JA                                 
057301              PERFORM IMS-GNP-WDK724                                      
057401                                                                          
057501              IF SEGMENT-FINNS                                            
057601                 SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX        
057701                 GIVING W-DAPRLIST                                        
057801                 MOVE MID-TIPRLIST-U   TO TMP1-YYMMDD                     
057901                 MOVE W-LISTDATUM      TO TMP2-YYMMDD                     
058001                 PERFORM WY2000P1                                         
058101                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
058201                    MOVE MFS-NUM-FAELT-FEL TO                             
058301                         MOD-TIPRLIST-U-ATTR                              
058401                    MOVE W-FEL-3           TO MOD-TEMFSFEL                
058501                    MOVE JA TO FLFEL-FAELT                                
058601                    MOVE JA TO FLSLUTA-LAS                                
058701                                                                          
058801                 ELSE                                                     
058901                                                                          
059001                    IF MID-TIPRLIST-U = W-LISTDATUM AND                   
059101                      MID-IDLEVNR = SPRL-IDLEVNR-PR                       
059201                      MOVE MFS-NUM-FAELT-RAETT TO                         
059301                           MOD-TIPRLIST-U-ATTR                            
059401                           MOD-IDLEVNR-ATTR                               
059501                      MOVE JA TO FLSLUTA-LAS                              
059601                    ELSE                                                  
059701                       ADD  1 TO INDX                                     
059801                    END-IF                                                
059901                                                                          
060001                 END-IF                                                   
060101              ELSE                                                        
060201                 MOVE MFS-NUM-FAELT-FEL TO                                
060301                      MOD-TIPRLIST-U-ATTR                                 
060401                      MOD-IDLEVNR-ATTR                                    
060501                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
060601                 MOVE JA TO FLFEL-FAELT                                   
060701              END-IF                                                      
060801                                                                          
060901           END-PERFORM                                                    
061001                                                                          
061101           IF INDX > 5                                                    
061201              MOVE MFS-NUM-FAELT-FEL TO                                   
061301                   MOD-TIPRLIST-U-ATTR                                    
061401                   MOD-IDLEVNR-ATTR                                       
061501              MOVE W-FEL-3           TO MOD-TEMFSFEL                      
061601              MOVE JA TO FLFEL-FAELT                                      
061701           END-IF                                                         
061801                                                                          
061901        END-IF                                                            
062001                                                                          
062101     ELSE                                                                 
062201        MOVE W-FEL-1 TO MOD-TEMFSFEL                                      
062301        PERFORM MFS-RENSA-FAELT-UTDATA                                    
062401        MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                            
062501        MOVE JA      TO FLFEL-FAELT                                       
062601     END-IF                                                               
062701     .                                                                    
062801     EJECT                                                                
062901 D-UPPDATERA SECTION.                                                     
063001                                                                          
063101     PERFORM IMS-GHU-WDK711                                               
063201                                                                          
063301     MOVE 1 TO INDX                                                       
063401     MOVE NEJ TO FLSLUTA-LAS                                              
063501     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                          
063601                   FLSLUTA-LAS = JA                                       
063701        PERFORM IMS-GHNP-WDK724                                           
063801        IF SEGMENT-FINNS                                                  
063901          SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
064001          GIVING W-DAPRLIST                                               
064101          MOVE W-LISTDATUM           TO W-TIPRLIST                        
064201                                                                          
064301          IF  MID-TIPRLIST-U = W-TIPRLIST                                 
064401          AND MID-IDLEVNR    = SPRL-IDLEVNR-PR                            
064501             PERFORM IMS-DLET-WDK7                                        
064601             MOVE JA         TO FLSLUTA-LAS                               
064701                                                                          
064801             MOVE SLAG-IDDC              TO PRTR-IDDC                     
064901             MOVE IDARTNR-WS             TO PRTR-IDARTNR                  
065001             MOVE 'CHN07'                TO PRTR-IDLEVNR                  
065101             MOVE W-TIPRLIST             TO PRTR-TIPRLIST                 
065201             MOVE 020                    TO PRTR-KDCALL                   
065301             CALL W510PRTR USING PRTR-W510PRTR WDK62-PCB                  
065401             IF PRTR-KDSVAR = '1'                                         
065501               PERFORM G-SKAPA-TRANS-5112                                 
065601               PERFORM H-SKICKA-TRANS                                     
065701             END-IF                                                       
065801          END-IF                                                          
065901        END-IF                                                            
066001     END-PERFORM                                                          
066101     .                                                                    
066201     EJECT                                                                
066301 F-BEHANDLA-BENAMNING SECTION.                                            
066401     SKIP3                                                                
066501     PERFORM IMS-GU-WDD311                                                
066601     IF SEGMENT-FINNS                                                     
066701        MOVE WDD3-TEXT-BEART TO MOD-BEART                                 
066801     ELSE                                                                 
066901        MOVE MFS-RENSA-FAELT TO MOD-BEART                                 
067001     END-IF                                                               
067101     .                                                                    
067201     EJECT                                                                
067301                                                                          
067401 G-SKAPA-TRANS-5112 SECTION.                                              
067501*                          INITIERA WMSGKOM                               
067601                                                                          
067701     MOVE   SPACE                TO MSG-KOM-WMSGKOM                       
067801     MOVE   +54                  TO MSG-KOM-KVLL                          
067901     MOVE   LOW-VALUE            TO MSG-KOM-KDZ1                          
068001     MOVE   LOW-VALUE            TO MSG-KOM-KDZ2                          
068101     MOVE   SPACE                TO MSG-KOM-KDTRANS                       
068201                                                                          
068301     MOVE   SPACE                TO MSG-KOM-IDMFSMED                      
068401                                    MSG-KOM-KDSVAR                        
068501                                                                          
068601     MOVE 'W5I11201'             TO MSG-KOM-IDCPYTXT                      
068701     MOVE 'DPRIS'                TO MSG-KOM-IDSNDNOD                      
068801     MOVE 'W5020700'             TO MSG-KOM-IDSNDJOB                      
068901                                                                          
069001     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
069101     ACCEPT MSG-KOM-TIKLOCK   FROM TIME                                   
069201                                                                          
069301     COMPUTE P-TO-P-LL = LENGTH OF 5112-MID-W5I11201 + 17                 
069401     MOVE LOW-VALUE               TO P-TO-P-Z1                            
069501     MOVE LOW-VALUE               TO P-TO-P-Z2                            
069601     MOVE 'W5T112X'               TO P-TO-P-TRANSKOD                      
069701*                                    MSG-KDTRANS-1                        
069801     MOVE '5112'                  TO P-TO-P-FROM-MID                      
069901     MOVE '1'                     TO P-TO-P-KDMFSFOR                      
070001                                                                          
070101     MOVE IDARTNR-WS              TO 5112-MID-IDARTNR-IN                  
070201                                                                          
070301****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
070401     MOVE '+++++++++'             TO 5112-MID-IDARTNR-UT                  
070501                                                                          
070601************************************************************              
070701                                                                          
070801     MOVE MID-TIPRLIST-U          TO WS-TIPRLIST-6                        
070901     MOVE WS-TIPRLIST-6           TO 5112-MID-TIPRLIST-U                  
071001                                                                          
071101     MOVE 'CHN07'                 TO 5112-MID-IDLEVNR                     
071201                                                                          
071301     MOVE 5112-MID-W5I11201       TO P-TO-P-DATA                          
071401     .                                                                    
071501     EJECT                                                                
071601                                                                          
071701 H-SKICKA-TRANS SECTION.                                                  
071801     CALL W006KOM USING MSG-PCB                                           
071901                        ALT-PCB                                           
072001                        KOMA-PCB                                          
072101                        MSG-KOM-WMSGKOM                                   
072201                        P-TO-P-AREA                                       
072301                                                                          
072801     .                                                                    
072901     EJECT                                                                
073001                                                                          
073101 S-BEHANDLA-BESTPRIS-INFO SECTION.                                        
073201     SKIP3                                                                
073301     MOVE 1 TO INDX                                                       
073401     PERFORM IMS-GNP-WDK724                                               
073501     PERFORM UNTIL INDX > 5 OR SEGMENT-SAKNAS                             
073601                                                                          
073701       IF SEGMENT-FINNS                                                   
073801         SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX                
073901         GIVING W-DAPRLIST                                                
074001         MOVE W-LISTDATUM           TO W-TIPRLIST                         
074101         MOVE W-TIPRLIST            TO MOD-TIPRLIST-PR (INDX)             
074201         MOVE SPRL-IDLEVNR-PR       TO MOD-IDLEVNR-PR (INDX)              
074301*        MOVE SPRL-PRARTBES-PR      TO MOD-PRARTBES-PR (INDX)             
074401         MOVE SPRL-PRARTBEL-PR      TO MOD-PRARTBEL-PR (INDX)             
074501         MOVE SPRL-PRARTBEL-PR      TO W-PRARTBEL                         
074601**** RÄKNA OM PRISRADER MED AKTUELL MÅNADSKURS                            
074701         MOVE WS-IDDC   TO W-IDDC-B6                                      
074801         PERFORM IMS-GU-WDB601                                            
074901         MOVE DCS-KDVALISO  TO CURR-KDVALISO-HUV                          
075001         MOVE SPRL-KDVALISO TO WS-KDVALISO                                
075101         IF CURR-KDVALISO-HUV = WS-KDVALISO                               
075201           MOVE 1           TO W-PRKURS                                   
075301           MOVE 1           TO W-REVALUTA                                 
075401         ELSE                                                             
075501           MOVE WS-KDVALISO TO CURR-KDVALISO-ROW                          
075802           MOVE 'M'         TO CURR-KDVALTYP                              
075803           MOVE W-DATE-AAMM TO CURR-TIAAMM                                
075805           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
075806           IF CURR-KDSVAR = ' '                                           
075807             MOVE CURR-PRKURS-NEW  TO W-PRKURS                            
075808             MOVE CURR-REVALUTA-TO TO W-REVALUTA                          
076201           ELSE                                                           
076402             MOVE 1         TO W-PRKURS                                   
076403             MOVE 1         TO W-REVALUTA                                 
076501           END-IF                                                         
076601         END-IF                                                           
076701         MOVE SPRL-IDLEVNR-PR     TO W-IDLEVNR                            
076801         PERFORM IMS-GU-WLLEVA01                                          
076901         IF SEGMENT-SAKNAS                                                
077001           MOVE 1 TO W-RETULF                                             
077101         ELSE                                                             
077201           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
077301           PERFORM IMS-GNP-WLLEVA11                                       
077401           IF SEGMENT-FINNS                                               
077501             IF LEV-TULL-TITULF < DAGENS-AAMMDD                           
077601               MOVE LEV-TULL-RETULF-1 TO W-RETULF                         
077701             ELSE                                                         
077801               MOVE LEV-TULL-RETULF-2 TO W-RETULF                         
077901             END-IF                                                       
078001           ELSE                                                           
078101             MOVE 1 TO W-RETULF                                           
078201           END-IF                                                         
078301         END-IF                                                           
078401         COMPUTE WS-PRARTBES-PR ROUNDED = W-PRARTBEL * W-RETULF           
078501                                        * W-PRKURS / W-REVALUTA           
078601         MOVE WS-PRARTBES-PR TO MOD-PRARTBES-PR (INDX)                    
078701                                                                          
078801****                                                                      
078901                                                                          
079001         IF SPRL-SUINLEV-PR > 0                                           
079101           MOVE 'DEL'    TO MOD-KDSTATUS-PR (INDX)                        
079201         ELSE                                                             
079301           MOVE 'EST'    TO MOD-KDSTATUS-PR (INDX)                        
079401         END-IF                                                           
079501         MOVE SPRL-KDPRURSP         TO MOD-KDPRURSP (INDX)                
079601         MOVE SPRL-KDVALISO         TO MOD-KDVALISO (INDX)                
079701         ADD 1 TO INDX                                                    
079801         IF INDX < 6                                                      
079901           PERFORM IMS-GNP-WDK724                                         
080001         END-IF                                                           
080101                                                                          
080201       END-IF                                                             
080301     END-PERFORM                                                          
080401     PERFORM UNTIL INDX > 5                                               
080501       MOVE MFS-RENSA-FAELT TO MOD-BEST-PRIS (INDX)                       
080601       ADD 1 TO INDX                                                      
080701     END-PERFORM                                                          
080801     .                                                                    
080901     EJECT                                                                
081001 MFS-ROER-EJ-FAELT-UTDATA SECTION.                                        
081101     SKIP3                                                                
081201     MOVE MFS-ROER-EJ-FAELT TO  MOD-BEART                                 
081301                                MOD-BEST-PRIS(1)                          
081401                                MOD-BEST-PRIS(2)                          
081501                                MOD-BEST-PRIS(3)                          
081601                                MOD-BEST-PRIS(4)                          
081701                                MOD-BEST-PRIS(5)                          
081801                                MOD-IDLEVNR                               
081901     .                                                                    
082001     SKIP3                                                                
082101 MFS-RENSA-FAELT-UTDATA SECTION.                                          
082201     SKIP3                                                                
082301                                                                          
082401     MOVE MFS-RENSA-FAELT  TO   MOD-BEART                                 
082501                                MOD-BEST-PRIS(1)                          
082601                                MOD-BEST-PRIS(2)                          
082701                                MOD-BEST-PRIS(3)                          
082801                                MOD-BEST-PRIS(4)                          
082901                                MOD-BEST-PRIS(5)                          
083001                                MOD-IDLEVNR                               
083101     .                                                                    
083201     EJECT                                                                
083301                                                                          
083401 IMS-GET-MSG SECTION.                                                     
083501     MOVE '  QC' TO GODK-STATUSKODER                                      
083601     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083801     PERFORM IMS-STATUSKONTROLL                                           
083901     .                                                                    
084001     SKIP3                                                                
084101                                                                          
084201 IMS-GN-MSG-KOM SECTION.                                                  
084301     MOVE '  QD' TO GODK-STATUSKODER                                      
084401     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
084501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084601     PERFORM IMS-STATUSKONTROLL                                           
084701     .                                                                    
084801     EJECT                                                                
084901                                                                          
085001 IMS-INSERT-MSG SECTION.                                                  
085101*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
085201*      MOVE '0' TO MFS-KDHUVOMR                                           
085301*    END-IF                                                               
085401     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085501     MOVE SPACE TO GODK-STATUSKODER                                       
085601     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085801     PERFORM IMS-STATUSKONTROLL                                           
085901     .                                                                    
086001     EJECT                                                                
086101                                                                          
086201 IMS-INSERT-MSG-KOM SECTION.                                              
086301     MOVE SPACE TO GODK-STATUSKODER                                       
086401     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA                          
086501     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
086601     PERFORM IMS-STATUSKONTROLL                                           
086701     .                                                                    
086801     EJECT                                                                
086901                                                                          
087001                                                                          
087101 IMS-GU-WDK601 SECTION.                                                   
087201     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
087301     DELIMITED  BY SIZE INTO SSA1                                         
087401     MOVE '  GE' TO GODK-STATUSKODER                                      
087501     CALL CBLTDLI USING GHU WDK6-PCB WDK601 SSA1                          
087601     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
087701     PERFORM IMS-STATUSKONTROLL                                           
087801     .                                                                    
087901     SKIP3                                                                
088001                                                                          
088101 IMS-GHU-WDK711 SECTION.                                                  
088201     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
088301     DELIMITED  BY SIZE INTO SSA1                                         
088401     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
088501     DELIMITED  BY SIZE INTO SSA2                                         
088601     MOVE '  GE' TO GODK-STATUSKODER                                      
088701     CALL CBLTDLI USING GHU WDK7-PCB WDK711 SSA1 SSA2                     
088801     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
088901     PERFORM IMS-STATUSKONTROLL                                           
089001     .                                                                    
089101     SKIP3                                                                
089201                                                                          
089301 IMS-GNP-WDK724 SECTION.                                                  
089401     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
089501     DELIMITED  BY SIZE INTO SSA1                                         
089601     MOVE 'WDK724   ' TO SSA2                                             
089701     MOVE '  GE' TO GODK-STATUSKODER                                      
089801     CALL CBLTDLI USING GNP WDK7-PCB WDK724 SSA1 SSA2                     
089901     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
090001     PERFORM IMS-STATUSKONTROLL                                           
090101     .                                                                    
090201     SKIP3                                                                
090301                                                                          
090401 IMS-GHNP-WDK724 SECTION.                                                 
090501     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
090601     DELIMITED  BY SIZE INTO SSA1                                         
090701     MOVE 'WDK724   ' TO SSA2                                             
090801     MOVE '  GE' TO GODK-STATUSKODER                                      
090901     CALL CBLTDLI USING GHNP WDK7-PCB WDK724 SSA1 SSA2                    
091001     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
091101     PERFORM IMS-STATUSKONTROLL                                           
091201     .                                                                    
091301     SKIP3                                                                
091401                                                                          
091501 IMS-DLET-WDK7 SECTION.                                                   
091601     MOVE '  GE' TO GODK-STATUSKODER                                      
091701     CALL CBLTDLI USING DLET WDK7-PCB WDK724                              
091801     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
091901     PERFORM IMS-STATUSKONTROLL                                           
092001     .                                                                    
092101     SKIP3                                                                
092201                                                                          
092301 IMS-GU-WDD311 SECTION.                                                   
092401     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
092501     DELIMITED BY SIZE INTO SSA1                                          
092601     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X    ')'                      
092701     DELIMITED BY SIZE INTO SSA2                                          
092801     MOVE '  GE' TO GODK-STATUSKODER                                      
092901     CALL CBLTDLI USING GU WDD3-PCB WDD3-TEXT-WDD311 SSA1 SSA2            
093001     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
093101     PERFORM IMS-STATUSKONTROLL                                           
093201     .                                                                    
093301     SKIP3                                                                
093401                                                                          
093501 IMS-GU-WLLEVA01 SECTION.                                                 
093601     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
093701     DELIMITED BY SIZE INTO SSA1                                          
093801     MOVE '  GE' TO GODK-STATUSKODER                                      
093901     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
094001     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
094101     PERFORM IMS-STATUSKONTROLL                                           
094201     .                                                                    
094301     SKIP3                                                                
094401                                                                          
094501 IMS-GNP-WLLEVA11 SECTION.                                                
094601     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
094701     DELIMITED BY SIZE INTO SSA1                                          
094801     MOVE '  GE' TO GODK-STATUSKODER                                      
094901     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
095001     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
095101     PERFORM IMS-STATUSKONTROLL                                           
095201     .                                                                    
095301     EJECT                                                                
095401                                                                          
095501 IMS-GU-WDB601    SECTION.                                                
095601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
095701          DELIMITED BY SIZE INTO SSA1                                     
095801     MOVE '  GE' TO GODK-STATUSKODER                                      
095901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
096001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
096101     PERFORM IMS-STATUSKONTROLL                                           
096201     IF SEGMENT-SAKNAS                                                    
096301         MOVE SPACE TO DCS-KDDC                                           
096401     END-IF                                                               
096501     .                                                                    
096601                                                                          
098101 IMS-STATUSKONTROLL SECTION.                                              
098201     SET STATUS-IX TO 1                                                   
098301     SEARCH GODK-STATUS AT END CALL FELLOG                                
098401       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
098501     END-SEARCH                                                           
098601     .                                                                    
098701     EJECT                                                                
098801*    -COPY WY2000P1                                                       
