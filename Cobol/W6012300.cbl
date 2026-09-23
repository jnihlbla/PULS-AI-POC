000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6012300.                                                
000300 AUTHOR.         ROS-MARIE CLASON > RAHUL JAIN.                           
000400 DATE-WRITTEN.   92/03/04 > JUL 2012.                                     
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ALLMÄN BESKRIVNING:                                              
000900*        PROGRAMMET ÄR EN MPP SOM VISAR ARTIKEL OCH/                      
001000*        ELLER PARTIINFORMATION AV VAD SOM FINNS PÅ                       
001100*        INLEVERANSREGISTRET.                                             
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001400*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001500*        PROGRAMMET LÄSER              WDB6                               
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W6T123                                              
001900*        MID:         W6I12301                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W6O12301                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700                                                                          
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6012300'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  IX2                         PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  WS-FLKLAR                   PIC X      VALUE SPACE.                  
004600                                                                          
004700*    --- RÄKNARE                                                          
004800 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
004900                                                                          
005000*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (943) MOD-LÄNGD + 4              
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +947 COMP SYNC.         
005200                                                                          
005300***********************************                                       
005400*        ARBETSFÄLT *                                                     
005500***********************************                                       
005600 01  WS-AREA.                                                             
005700                                                                          
005800     03  STATUS-TAB OCCURS 11.                                            
005900         05  STATUS-JA            PIC X(1)  VALUE 'N'.                    
006000                                                                          
006100     03  WS-IDPRT.                                                        
006200         05  WS-IDPRT1            PIC X(2).                               
006300         05  WS-IDPRT2            PIC X(4).                               
006400         05  WS-IDPRT3            PIC X(2).                               
006500                                                                          
006600     03  WS-IDLOPNRM              PIC X(9).                               
006700                                                                          
006800     03  WS-ADGANG                PIC 9(3).                               
006900     03  WS-ADGANG-X REDEFINES WS-ADGANG.                                 
007000         05  FILLER               PIC X(1).                               
007100         05  WS-RED-ADGANG        PIC X(2).                               
007200                                                                          
007300     03  WS-ADLAGOMR              PIC 9(3).                               
007400     03  WS-ADLAGOMR-X REDEFINES WS-ADLAGOMR.                             
007500         05  FILLER               PIC X(1).                               
007600         05  WS-RED-ADLAGOMR      PIC X(2).                               
007700                                                                          
007800     03  WS-TEST-ADGANG             PIC 9(3).                             
007900     03  WS-TEST-ADGANG-X REDEFINES WS-TEST-ADGANG.                       
008000         05  FILLER                 PIC X(1).                             
008100         05  WS-TEST-RED-ADGANG     PIC X(2).                             
008200                                                                          
008300     03  WS-TEST-ADLAGOMR           PIC 9(3).                             
008400     03  WS-TEST-ADLAGOMR-X REDEFINES WS-TEST-ADLAGOMR.                   
008500         05  FILLER                 PIC X(1).                             
008600         05  WS-TEST-RED-ADLAGOMR   PIC X(2).                             
008700                                                                          
008800     03  WS-TEST-ADPLATS            PIC 9(5).                             
008900                                                                          
009000     03  WS-TEST-KDLAGEMB           PIC X(4).                             
009100                                                                          
009200     03  WS-IDRADNR               PIC 9(5).                               
009300     03  WS-IDRADNR-X REDEFINES WS-IDRADNR.                               
009400         05  FILLER               PIC X(2).                               
009500         05  WS-RED-IDRADNR       PIC X(3).                               
009600                                                                          
009700     03  WS-KVINLART              PIC S9(7).                              
009800                                                                          
009900     03  WS-IDLEVNR-NEW           PIC X(5) VALUE SPACE.                   
010000                                                                          
010100     03  WS-SPAR-IDLOPNRM         PIC 9(9) VALUE ZERO.                    
010200     03  WS-SPAR-IDLEVNR          PIC X(5) VALUE SPACE.                   
010300     03  WS-SPAR-PRARTSTD         PIC 9(7)V9(2).                          
010400     03  WS-SPAR-FLFRD            PIC X(1).                               
010500                                                                          
010600     03  WS-IDLOPNRM-CHECK        PIC 9(9).                               
010700                                                                          
010800     03  SPAR-TIINLMOT            PIC 9(6).                               
010900     03  SPAR-IDARTNR             PIC 9(9).                               
011000     03  SPAR-IDLOPNRM            PIC 9(9) VALUE ZERO.                    
011100     03  SPAR-VKART               PIC 9(7).                               
011200     03  SPAR-VKART-KG            PIC 9(4)V9(3).                          
011300     03  SPAR-ADLAGOMR            PIC 9(2).                               
011400     03  SPAR-ADGANG              PIC 9(2).                               
011500     03  SPAR-ADPLATS             PIC 9(5).                               
011600     03  SPAR-KDSORT              PIC X(2).                               
011700     03  SPAR-BEART               PIC X(25).                              
011800     03  SPAR-IDDC                PIC X(2).                               
011900     03  SPAR-BEFT                PIC 9(2).                               
012000     03  SPAR-KDINLSTA-NEW        PIC X(3).                               
012100     03  SPAR-KDINLSTA-OLD        PIC X(3).                               
012200     03  SPAR-RAD  OCCURS 14.                                             
012300         05  SPAR-IDLEVNR-KOLLI   PIC X(5)   VALUE SPACE.                 
012400         05  SPAR-IDOKOLLI        PIC 9(9).                               
012500                                                                          
012600     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
012700     03  W-PTOP2-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
012800     03  W-PTOP3-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
012900                                                                          
013000*----TILL W611FRD                                                         
013100     03  WS-SPAR-ADINLOMR-OLD       PIC X(4) VALUE SPACE.                 
013200     03  WS-SPAR-ADINLOMR-NXT-OLD   PIC X(4) VALUE SPACE.                 
013300                                                                          
013400*                                                                         
013500*    --- ARBETSFÄLT FÖR SWITCHAR                                          
013600                                                                          
013700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013800     88  INDATA-OK                           VALUE 'J'.                   
013900     88  INDATA-FEL                          VALUE 'N'.                   
014000                                                                          
014100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014200     88  NYCKLAR-OK                          VALUE 'J'.                   
014300     88  NYCKLAR-FEL                         VALUE 'N'.                   
014400                                                                          
014500 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
014600     88  ARTNR-OK                            VALUE 'J'.                   
014700                                                                          
014800*----------------------------------------------------------------*        
014900*   NKLTYP1=PARTINR, NKLTYP2=ARTNR, NKLTYP3=ARTNR-LEVNR-FS                
015000*----------------------------------------------------------------*        
015100 77  NKLTYP-SW                  PIC X.                                    
015200     88  NKLTYP1                            VALUE '1'.                    
015300     88  NKLTYP2                            VALUE '2'.                    
015400     88  NKLTYP3                            VALUE '3'.                    
015500                                                                          
015600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015700     88  EGEN-MID                            VALUE '6123'.                
015800     88  GODK-MID                            VALUE '6121' '6122'          
015900                                                   '6123'.                
016000     88  HELP-MID                            VALUE '0551'.                
016100     EJECT                                                                
016200 01  WS-IDMSG-ERROR              PIC X(3).                                
016300     88  WRONG-KEY                           VALUE '022'.                 
016400     EJECT                                                                
016500                                                                          
016600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016700 01  GENERELLA-SUBPROGRAM.                                                
016800     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
016900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017100     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
017200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
017300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017400     03  W6012310                PIC X(8)    VALUE 'W6012310'.            
017500     EJECT                                                                
017600                                                                          
017700*01 -COPY WMSGINIT                                                        
017800     SKIP3                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018000*01 -COPY WMEDAREA                                                        
018100     SKIP3                                                                
018200 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
018300 01  REQU-AREA.                                                           
018400*    03 -COPY WZ01REQU                                                    
018500*    03 -COPY W60123I1                                                    
018600     SKIP3                                                                
018700*                                                                         
018800 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
018900 01  RESP-AREA.                                                           
019000*    03 -COPY WZ01RESP                                                    
019100*    03 -COPY W60123O1                                                    
019200     SKIP3                                                                
019300 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +11.               
019400 77  OUT-KVRADER                 PIC S9(4)  COMP.                         
019500     EJECT                                                                
019600                                                                          
019700*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
019800*    --- COPYTEXTER FÖR W006PRT, W611PMRK, W60191                         
019900*                                                                         
020000*01  -COPY W006PRT                                                        
020100     EJECT                                                                
020200*                                                                         
020300*01  -COPY W611PMRK                                                       
020400     EJECT                                                                
020500                                                                          
020600 01  P-TO-P-T91.                                                          
020700*----TILL W60191                                                          
020800     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
020900     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
021000     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
021100     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
021200     03  FILLER                PIC X(1)    VALUE SPACE.                   
021300     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6123'.                  
021400     03  PTOP1-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
021500*    03  MID -COPY W6I19101       -PRE T91-                               
021600     EJECT                                                                
021700*                                                                         
021800 01  P-TO-P-T94.                                                          
021900*----TILL W60194                                                          
022000     03  PTOP2-LL              PIC S9(4)   COMP SYNC.                     
022100     03  PTOP2-Z1              PIC X(1)    VALUE LOW-VALUE.               
022200     03  PTOP2-Z2              PIC X(1)    VALUE LOW-VALUE.               
022300     03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W6T194X'.               
022400     03  FILLER                PIC X(1)    VALUE SPACE.                   
022500     03  PTOP2-IDTRANS         PIC X(4)    VALUE '6123'.                  
022600     03  PTOP2-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
022700*    03  MID -COPY W6I19401       -PRE T94-                               
022800     EJECT                                                                
022900*                                                                         
023000 01  P-TO-P-T95.                                                          
023100*----TILL W60195                                                          
023200     03  PTOP3-LL              PIC S9(4)   COMP SYNC.                     
023300     03  PTOP3-Z1              PIC X(1)    VALUE LOW-VALUE.               
023400     03  PTOP3-Z2              PIC X(1)    VALUE LOW-VALUE.               
023500     03  PTOP3-TRANSKOD        PIC X(7)    VALUE 'W6T195X'.               
023600     03  FILLER                PIC X(1)    VALUE SPACE.                   
023700     03  PTOP3-IDTRANS         PIC X(4)    VALUE '6123'.                  
023800     03  PTOP3-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
023900*    03  MID -COPY W6I19501       -PRE T95-                               
024000     EJECT                                                                
024100                                                                          
024200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024300*                                                                         
024400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024500     SKIP3                                                                
024600*01  MID -COPY W6I12301                                                   
024700     EJECT                                                                
024800                                                                          
024900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025000     SKIP3                                                                
025100*01  -COPY WMSGAREA                                                       
025200     EJECT                                                                
025300                                                                          
025400     03  MOD REDEFINES MSG-AREA.                                          
025500*      05  -COPY W6O12301                                                 
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025900     SKIP3                                                                
026000*01  -COPY WMFSAREA                                                       
026100     EJECT                                                                
026200 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
026300     SKIP3                                                                
026400*01  -COPY WL01MCNV                                                       
026500     EJECT                                                                
026600                                                                          
026700*    --- STATUS-KOD FRÅN IMS                                              
026800 01  STATUS-WS                   PIC XX.                                  
026900     88  SEGMENT-FINNS                       VALUE '  '.                  
027000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027200     SKIP2                                                                
027300                                                                          
027400 01  GODK-STATUSKODER.                                                    
027500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027600     SKIP3                                                                
027700                                                                          
027800 01  SSA1                        PIC X(90).                               
027900 01  SSA2                        PIC X(64).                               
028000 01  SSA3                        PIC X(64).                               
028100     EJECT                                                                
028200                                                                          
028300*    --- IMS FUNKTIONSKODER                                               
028400*01  -COPY W0003                                                          
028500     EJECT                                                                
028600                                                                          
028700*    ---  DLI INPUT-OUTPUT AREA                                           
028800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028900     SKIP3                                                                
029000                                                                          
029100 01  DLI-IO-AREA-1.                                                       
029200     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
029300     SKIP3                                                                
029400                                                                          
029500     03  W6INLA01 REDEFINES IO-AREA-1.                                    
029600*        05  -COPY W6D101                                                 
029700     SKIP3                                                                
029800                                                                          
029900     03  W6INLA11 REDEFINES IO-AREA-1.                                    
030000*        05  -COPY W6D111                                                 
030100     SKIP3                                                                
030200                                                                          
030300     03  W6INLA21 REDEFINES IO-AREA-1.                                    
030400*        05  -COPY W6D121                                                 
030500     SKIP3                                                                
030600                                                                          
030700     03  W6INLH11 REDEFINES IO-AREA-1.                                    
030800*        05  -COPY W6D1H1                                                 
030900     SKIP3                                                                
031000                                                                          
031100     03  W6INLB11 REDEFINES IO-AREA-1.                                    
031200*        05  -COPY W6D1B1                                                 
031300     SKIP3                                                                
031400                                                                          
031500 01  DLI-IO-AREA-2.                                                       
031600     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
031700     SKIP3                                                                
031800                                                                          
031900     03  W6LOPA01 REDEFINES IO-AREA-2.                                    
032000*        05  -COPY W6GX01                                                 
032100     SKIP3                                                                
032200                                                                          
032300     03  W6LOPA11 REDEFINES IO-AREA-2.                                    
032400*        05  -COPY W6GX6018                                               
032500                                                                          
032600 01  DLI-IO-AREA-3.                                                       
032700     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
032800     SKIP3                                                                
032900                                                                          
033000     03  W6PLAA01 REDEFINES IO-AREA-3.                                    
033100*        05  -COPY W6GX01                                                 
033200     SKIP3                                                                
033300                                                                          
033400     03  W6PLAA11 REDEFINES IO-AREA-3.                                    
033500*        05  -COPY W6GX6006                                               
033600                                                                          
033700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033800 01   DLI-IO-AREA-B601.                                                   
033900*     03  -COPY WDB601                                                    
034000                                                                          
034100     EJECT                                                                
034200                                                                          
034300 LINKAGE SECTION.                                                         
034400                                                                          
034500*01  -COPY W0009   -PRE MSG-                                              
034600     EJECT                                                                
034700                                                                          
034800*01  -COPY W0009   -PRE ALT1-                                             
034900     EJECT                                                                
035000                                                                          
035100*01  -COPY W0009   -PRE ALT2-                                             
035200     EJECT                                                                
035300                                                                          
035400*01  -COPY W0009   -PRE ALT3-                                             
035500     EJECT                                                                
035600                                                                          
035700*01  -COPY W0008  -PRE USEA-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000                                                                          
036100*01  -COPY W0008  -PRE INLA-                                              
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400                                                                          
036500*01  -COPY W0008  -PRE INLB1-                                             
036600     05  FILLER                  PIC X.                                   
036700     EJECT                                                                
036800                                                                          
036900*01  -COPY W0008  -PRE INLB-                                              
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200                                                                          
037300*01  -COPY W0008  -PRE INLH1-                                             
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600                                                                          
037700*01  -COPY W0008  -PRE LOPA-                                              
037800     05  FILLER                  PIC X.                                   
037900     EJECT                                                                
038000                                                                          
038100*01  -COPY W0008  -PRE PLAA-                                              
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400                                                                          
038500*01  -COPY W0008  -PRE INLB-PMRK-                                         
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800                                                                          
038900*01  -COPY W0008  -PRE INLC-PMRK-                                         
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200                                                                          
039300*01  -COPY W0008  -PRE WDB6-                                              
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600                                                                          
039700*01  -COPY W0008  -PRE BENA-                                              
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
040000                                                                          
040100 PROCEDURE DIVISION  USING MSG-PCB                                        
040200                           ALT1-PCB                                       
040300                           ALT2-PCB                                       
040400                           ALT3-PCB                                       
040500                           USEA-PCB                                       
040600                           INLA-PCB                                       
040700                           INLB1-PCB                                      
040800                           INLB-PCB                                       
040900                           INLH1-PCB                                      
041000                           LOPA-PCB                                       
041100                           PLAA-PCB                                       
041200                           INLB-PMRK-PCB                                  
041300                           INLC-PMRK-PCB                                  
041400                           WDB6-PCB                                       
041500                           BENA-PCB.                                      
041600                                                                          
041700     ENTRY 'DLITCBL' USING MSG-PCB                                        
041800                           ALT1-PCB                                       
041900                           ALT2-PCB                                       
042000                           ALT3-PCB                                       
042100                           USEA-PCB                                       
042200                           INLA-PCB                                       
042300                           INLB1-PCB                                      
042400                           INLB-PCB                                       
042500                           INLH1-PCB                                      
042600                           LOPA-PCB                                       
042700                           PLAA-PCB                                       
042800                           INLB-PMRK-PCB                                  
042900                           INLC-PMRK-PCB                                  
043000                           WDB6-PCB                                       
043100                           BENA-PCB.                                      
043200                                                                          
043300     EJECT                                                                
043400                                                                          
043500*----------------------------------------------------------------*        
043600     PERFORM IMS-GET-MSG                                                  
043700     IF SEGMENT-FINNS                                                     
043800        PERFORM A-INIT                                                    
043900        PERFORM B-INIT-KEYS                                               
044000        IF NYCKLAR-OK                                                     
044100           IF MFS-UPDATE                                                  
044200              SET REQU-UPDATE TO TRUE                                     
044300           ELSE                                                           
044400              IF MFS-UPD-V                                                
044500                 SET REQU-UPD-V TO TRUE                                   
044600              ELSE                                                        
044700                 IF MFS-FIRST                                             
044800                    SET REQU-FIRST TO TRUE                                
044900                    PERFORM MFS-RENSA-FAELT-UT                            
045000                 ELSE                                                     
045100                    IF MFS-NEXT                                           
045200                       SET REQU-NEXT TO TRUE                              
045300                       PERFORM D-NAESTA-SIDA                              
045400                    ELSE                                                  
045500                       SET REQU-QUERY TO TRUE                             
045600                       PERFORM E-SAMMA-SIDA                               
045700                    END-IF                                                
045800                 END-IF                                                   
045900              END-IF                                                      
046000           END-IF                                                         
046100        END-IF                                                            
046200        PERFORM F-CALL-BIZ-LOGIC-W6012310                                 
046300        PERFORM S90-BLANKUTF-NUM-FAELT                                    
046400        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O12301-CTX + 4                 
046500        PERFORM IMS-INSERT-MSG                                            
046600     END-IF                                                               
046700                                                                          
046800     MOVE ZERO TO RETURN-CODE                                             
046900     GOBACK                                                               
047000     .                                                                    
047100     EJECT                                                                
047200*----------------------------------------------------------------*        
047300 A-INIT SECTION.                                                          
047400                                                                          
047500     IF MSG-DUBBLA-TRANSKODER                                             
047600        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I12301                
047700        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
047800        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
047900     ELSE                                                                 
048000        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I12301                 
048100        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
048200        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
048300     END-IF                                                               
048400                                                                          
048500     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
048600     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
048700     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
048800                                                                          
048900     MOVE LOW-VALUE         TO MSG-AREA                                   
049000     MOVE 'W6O123N1'        TO MFS-IDMOD                                  
049100     MOVE '6123'            TO MOD-IDTRANS                                
049200     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL MOD-TEMFSINF                  
049300                                                                          
049400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049500     MOVE '001'             TO MSGI-KDCALL                                
049600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049800     MOVE '6123'            TO MSGI-IDTRANS                               
049900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050000                                                                          
050100     IF EGEN-MID OR HELP-MID                                              
050200        CONTINUE                                                          
050300     ELSE                                                                 
050400        MOVE SPACE TO MFS-KDTRTYP                                         
050500        MOVE '7' TO MFS-IDPFK                                             
050600     END-IF                                                               
050700                                                                          
050800     IF MSGI-IDLAND-SPR = 'GB'                                            
050900       MOVE 'GB'                    TO MCNV-IDSPRAK                       
051000     ELSE                                                                 
051100       MOVE 'SV'                    TO MCNV-IDSPRAK                       
051200     END-IF                                                               
051300                                                                          
051400     MOVE '101'                     TO REQU-IDMSGVER                      
051500     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
051600     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
051700     MOVE MAX-KVRADER               TO REQU-KVRADER                       
051800     .                                                                    
051900     EJECT                                                                
052000*----------------------------------------------------------------*        
052100 B-INIT-KEYS SECTION.                                                     
052200                                                                          
052300     MOVE JA                 TO NYCKLAR-SW                                
052400     MOVE SPACE              TO NKLTYP-SW                                 
052500*----NKL-FÄLT-UT                                                          
052600     INSPECT MID-IDLOPNRM-IN REPLACING LEADING SPACE BY ZERO              
052700     INSPECT MID-IDARTNR-IN  REPLACING LEADING SPACE BY ZERO              
052800     INSPECT MID-IDLOPNRM-UT REPLACING LEADING SPACE BY ZERO              
052900     INSPECT MID-IDARTNR-UT  REPLACING LEADING SPACE BY ZERO              
053000                                                                          
053100     IF GODK-MID                                                          
053200        IF MID-IDLOPNRM-IN NUMERIC AND MID-IDLOPNRM-IN > +0               
053300           MOVE MID-IDLOPNRM-IN    TO MSGI-IDLOPNRM                       
053400                                      REQU-IDLOPNRM-KEY                   
053500        ELSE                                                              
053600           IF MID-IDLOPNRM-UT NUMERIC AND MID-IDLOPNRM-UT > +0            
053700              MOVE MID-IDLOPNRM-UT TO MSGI-IDLOPNRM                       
053800                                      REQU-IDLOPNRM-KEY                   
053900           ELSE                                                           
054000             MOVE +0               TO MSGI-IDLOPNRM                       
054100                                      REQU-IDLOPNRM-KEY                   
054200           END-IF                                                         
054300        END-IF                                                            
054400                                                                          
054500        IF MID-IDARTNR-IN NUMERIC AND MID-IDARTNR-IN > +0                 
054600           MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                        
054700                                      REQU-IDARTNR-KEY                    
054800        ELSE                                                              
054900           IF MID-IDARTNR-UT NUMERIC AND MID-IDARTNR-UT > +0              
055000              MOVE MID-IDARTNR-UT  TO MSGI-IDARTNR                        
055100                                      REQU-IDARTNR-KEY                    
055200           ELSE                                                           
055300              MOVE +0              TO MSGI-IDARTNR                        
055400                                      REQU-IDARTNR-KEY                    
055500           END-IF                                                         
055600        END-IF                                                            
055700                                                                          
055800        MOVE MID-IDLEVNR-IN      TO MSGI-IDLEVNR                          
055900                                    REQU-IDLEVNR-KEY                      
056000        MOVE MID-IDFS-IN         TO MSGI-IDFS                             
056100                                    REQU-IDFS-KEY                         
056200        MOVE MID-IDLBBET-IN      TO MSGI-IDLBBET                          
056300                                    REQU-IDLBBET-KEY                      
056400        MOVE MID-IDDC-IN         TO MSGI-IDDC                             
056500                                    REQU-IDDC-KEY                         
056600        MOVE MID-ADINLOMR-PRT    TO MSGI-ADINLOMR-PRT                     
056700                                    REQU-ADINLOMR-PRT                     
056800     ELSE                                                                 
056900        IF MID-IDARTNR-IN = ALL '+'                                       
057000          IF MID-IDARTNR-UT NUMERIC                                       
057100            MOVE MID-IDARTNR-UT   TO MSGI-IDARTNR                         
057200                                     REQU-IDARTNR-KEY                     
057300            MOVE ALL '+'          TO MID-IDLOPNRM-IN                      
057400                                     MID-IDLEVNR-IN                       
057500                                     MID-IDFS-IN                          
057600                                     MID-IDLBBET-IN                       
057700                                     MID-ADINLOMR-PRT                     
057800            MOVE SPACE            TO MID-IDLOPNRM-UT                      
057900                                     MID-IDLEVNR-UT                       
058000                                     MID-IDFS-UT                          
058100                                     MID-IDLBBET-UT                       
058200          END-IF                                                          
058300        ELSE                                                              
058400          IF MID-IDARTNR-IN NUMERIC                                       
058500            MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                         
058600                                     REQU-IDARTNR-KEY                     
058700            MOVE ALL '+'          TO MID-IDLOPNRM-IN                      
058800                                     MID-IDLEVNR-IN                       
058900                                     MID-IDFS-IN                          
059000                                     MID-IDLBBET-IN                       
059100                                     MID-ADINLOMR-PRT                     
059200            MOVE SPACE            TO MID-IDLOPNRM-UT                      
059300                                     MID-IDLEVNR-UT                       
059400                                     MID-IDFS-UT                          
059500                                     MID-IDLBBET-UT                       
059600          END-IF                                                          
059700        END-IF                                                            
059800     END-IF                                                               
059900                                                                          
060000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
060100                                                                          
060200     IF MID-IDLOPNRM-IN = ALL '+' AND                                     
060300        MID-IDARTNR-IN  = ALL '+' AND                                     
060400        MID-IDLEVNR-IN  = ALL '+' AND                                     
060500        MID-IDFS-IN     = ALL '+'                                         
060600        IF MID-IDLOPNRM-UT = ZERO  AND                                    
060700           MID-IDARTNR-UT  = ZERO  AND                                    
060800           MID-IDLEVNR-UT  = SPACE AND                                    
060900           MID-IDFS-UT     = SPACE                                        
061000*-FEL - 401                                                               
061100           MOVE NEJ          TO NYCKLAR-SW                                
061200           MOVE '7'          TO MFS-IDPFK                                 
061300           MOVE SPACE        TO MFS-KDTRTYP                               
061400        ELSE                                                              
061500*----------GAMLA NYCKLAR ELLER PF-HOPP                                    
061600           PERFORM BB-FORMELLA-KONTR                                      
061700           PERFORM BAD-KONTR-IDDC                                         
061800        END-IF                                                            
061900     ELSE                                                                 
062000*-------NYA NYCKLAR                                                       
062100        MOVE SPACE TO MFS-KDTRTYP                                         
062200        MOVE '7'   TO MFS-IDPFK                                           
062300        PERFORM BA-FORMELLA-KONTR                                         
062400     END-IF                                                               
062500                                                                          
062600     IF MSGI-ADINLOMR-PRT NOT = ALL '+' AND                               
062700        MSGI-ADINLOMR-PRT NOT = SPACE                                     
062800        MOVE MSGI-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                    
062900                                      REQU-ADINLOMR-PRT                   
063000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR               
063100     END-IF                                                               
063200                                                                          
063300     IF MSGI-IDLBBET    NOT = ALL '+' AND                                 
063400        MSGI-IDLBBET    NOT = SPACE                                       
063500        MOVE MSGI-IDLBBET           TO MOD-IDLBBET-UT                     
063600                                       REQU-IDLBBET-KEY                   
063700     END-IF                                                               
063800                                                                          
063900     IF GODK-MID OR HELP-MID                                              
064000        CONTINUE                                                          
064100     ELSE                                                                 
064200        PERFORM BC-KOLLA-ARTNR-HOPP                                       
064300        IF ARTNR-OK                                                       
064400          MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                         
064500                                  MOD-IDLEVNR-UT                          
064600                                  MOD-IDFS-UT                             
064700                                  MOD-IDLBBET-UT                          
064800                                  MOD-ADINLOMR-PRT                        
064900                                  MOD-TEMFSFEL                            
065000        ELSE                                                              
065100          MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                         
065200                                  MOD-IDARTNR-UT                          
065300                                  MOD-IDLEVNR-UT                          
065400                                  MOD-IDFS-UT                             
065500                                  MOD-IDLBBET-UT                          
065600                                  MOD-ADINLOMR-PRT                        
065700                                  MOD-IDDC-UT                             
065800                                  MOD-TEMFSFEL                            
065900          PERFORM MFS-RENSA-FAELT-UT                                      
066000        END-IF                                                            
066100     END-IF                                                               
066200     IF EGEN-MID                                                          
066300        MOVE MID-BEART              TO REQU-BEART                         
066400        MOVE MID-KDLAGEMB           TO REQU-KDLAGEMB                      
066500        MOVE MID-ADLAGOMR           TO REQU-ADLAGOMR                      
066600        MOVE MID-ADGANG             TO REQU-ADGANG                        
066700        MOVE MID-ADPLATS            TO REQU-ADPLATS                       
066800        MOVE MID-KDSORT             TO REQU-KDSORT                        
066900        MOVE MID-KDFARLIG-TXT       TO REQU-KDFARLIG-TXT                  
067000                                                                          
067100        MOVE ZERO                   TO IX                                 
067200        ADD 1                       TO IX                                 
067300        PERFORM UNTIL IX > MAX-KVRADER                                    
067400           MOVE MID-IDLOPNRM(IX)    TO REQU-IDLOPNRM(IX)                  
067500           MOVE MID-IDRADNR(IX)     TO REQU-IDRADNR(IX)                   
067600           MOVE MID-IDLEVNR(IX)     TO REQU-IDLEVNR(IX)                   
067700           MOVE MID-IDOKOLLI(IX)    TO REQU-IDOKOLLI(IX)                  
067800           MOVE MID-KDINLSTA(IX)    TO REQU-KDINLSTA(IX)                  
067900           MOVE MID-KDCMDVAL-INPUT(IX)                                    
068000                                    TO REQU-KDCMDVAL-INPUT(IX)            
068100        ADD 1                       TO IX                                 
068200        END-PERFORM                                                       
068300     END-IF                                                               
068400     PERFORM MFS-RENSA-FAELT-IN                                           
068500     .                                                                    
068600     EJECT                                                                
068700*----------------------------------------------------------------*        
068800 BA-FORMELLA-KONTR SECTION.                                               
068900                                                                          
069000*----PARTINR                                                              
069100     IF MID-IDLOPNRM-IN NOT = ALL '+' AND                                 
069200        MID-IDARTNR-IN    = ALL '+' AND                                   
069300        MID-IDLEVNR-IN    = ALL '+' AND                                   
069400        MID-IDFS-IN       = ALL '+'                                       
069500        PERFORM BAA-KONTR-PARTINR                                         
069600     ELSE                                                                 
069700*-------ARTNR                                                             
069800        IF MID-IDLOPNRM-IN     = ALL '+' AND                              
069900           MID-IDARTNR-IN  NOT = ALL '+' AND                              
070000           MID-IDLEVNR-IN      = ALL '+' AND                              
070100           MID-IDFS-IN         = ALL '+'                                  
070200           PERFORM BAB-KONTR-ARTNR                                        
070300        ELSE                                                              
070400*----------ARTNR,LEVNR,FS                                                 
070500           IF (MID-IDLOPNRM-IN     = ALL '+') AND                         
070600              (MID-IDARTNR-IN  NOT = ALL '+' OR                           
070700              MID-IDLEVNR-IN  NOT = ALL '+' OR                            
070800              MID-IDFS-IN     NOT = ALL '+')                              
070900              PERFORM BAC-KONTR-ART-LEV-FS                                
071000           ELSE                                                           
071100*-FEL - 401                                                               
071200              MOVE NEJ             TO NYCKLAR-SW                          
071300                                                                          
071400              IF MID-IDLOPNRM-IN NOT = ALL '+'                            
071500                 MOVE MSGI-IDLOPNRM    TO MOD-IDLOPNRM-UT                 
071600                                          REQU-IDLOPNRM-KEY               
071700              END-IF                                                      
071800              IF MID-IDARTNR-IN NOT = ALL '+'                             
071900                 MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                   
072000                                         REQU-IDARTNR-KEY                 
072100              END-IF                                                      
072200              IF MID-IDLEVNR-IN NOT = ALL '+'                             
072300                 MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                   
072400                                         REQU-IDLEVNR-KEY                 
072500              END-IF                                                      
072600              IF MID-IDFS-IN NOT = ALL '+'                                
072700                 MOVE MSGI-IDFS    TO MOD-IDFS-UT                         
072800                                      REQU-IDFS-KEY                       
072900              END-IF                                                      
073000           END-IF                                                         
073100        END-IF                                                            
073200     END-IF                                                               
073300     PERFORM BAD-KONTR-IDDC                                               
073400     PERFORM MFS-RENSA-FAELT-UT                                           
073500     .                                                                    
073600     EJECT                                                                
073700*----------------------------------------------------------------*        
073800 BAA-KONTR-PARTINR SECTION.                                               
073900                                                                          
074000     IF (MID-IDLOPNRM-IN NOT = ALL '+') AND                               
074100        (MID-IDLOPNRM-IN = ZERO OR                                        
074200         MID-IDLOPNRM-IN NOT NUMERIC)                                     
074300*-FEL - 401                                                               
074400        MOVE NEJ                   TO NYCKLAR-SW                          
074500        MOVE MID-IDLOPNRM-IN       TO MOD-IDLOPNRM-UT                     
074600                                      REQU-IDLOPNRM-KEY                   
074700     ELSE                                                                 
074800        MOVE '1'                   TO NKLTYP-SW                           
074900                                                                          
075000        IF MSGI-IDLOPNRM    > ZERO                                        
075100*-------NY    NKL - PARTINR                                               
075200           MOVE MSGI-IDLOPNRM      TO MOD-IDLOPNRM-UT                     
075300                                      REQU-IDLOPNRM-KEY                   
075400           MOVE SPACE              TO REQU-IDARTNR-KEY                    
075500                                      REQU-IDLEVNR-KEY                    
075600                                      REQU-IDFS-KEY                       
075700        END-IF                                                            
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100*----------------------------------------------------------------*        
076200 BAB-KONTR-ARTNR SECTION.                                                 
076300                                                                          
076400     IF (MID-IDARTNR-IN NOT = ALL '+') AND                                
076500        (MID-IDARTNR-IN = ZERO OR                                         
076600         MID-IDARTNR-IN NOT NUMERIC)                                      
076700*-FEL - 401                                                               
076800        MOVE NEJ                   TO NYCKLAR-SW                          
076900        MOVE MID-IDARTNR-IN        TO MOD-IDARTNR-UT                      
077000                                      REQU-IDARTNR-KEY                    
077100     ELSE                                                                 
077200        MOVE '2'                   TO NKLTYP-SW                           
077300        MOVE MSGI-IDARTNR          TO MOD-IDARTNR-UT                      
077400                                      REQU-IDARTNR-KEY                    
077500        MOVE SPACE                 TO REQU-IDLOPNRM-KEY                   
077600                                      REQU-IDLEVNR-KEY                    
077700                                      REQU-IDFS-KEY                       
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100*----------------------------------------------------------------*        
078200 BAC-KONTR-ART-LEV-FS SECTION.                                            
078300                                                                          
078400     IF (MID-IDARTNR-IN NOT = ALL '+') AND                                
078500        (MID-IDARTNR-IN = ZERO OR                                         
078600         MID-IDARTNR-IN NOT NUMERIC)                                      
078700*-FEL - 401                                                               
078800         MOVE NEJ               TO NYCKLAR-SW                             
078900     END-IF                                                               
079000     IF (MID-IDLEVNR-IN NOT = ALL '+') AND                                
079100        (MID-IDLEVNR-IN = SPACE)                                          
079200*-FEL - 401                                                               
079300         MOVE NEJ               TO NYCKLAR-SW                             
079400     END-IF                                                               
079500                                                                          
079600     MOVE MSGI-IDARTNR          TO MOD-IDARTNR-UT                         
079700                                   REQU-IDARTNR-KEY                       
079800     MOVE MSGI-IDLEVNR          TO MOD-IDLEVNR-UT                         
079900                                   REQU-IDLEVNR-KEY                       
080000     MOVE MSGI-IDFS             TO MOD-IDFS-UT                            
080100                                   REQU-IDFS-KEY                          
080200     MOVE ALL '+'               TO REQU-IDLOPNRM-KEY                      
080300                                                                          
080400     IF NYCKLAR-FEL                                                       
080500        CONTINUE                                                          
080600     ELSE                                                                 
080700        MOVE '3'                  TO NKLTYP-SW                            
080800        PERFORM BACA-KONTR-FORTS                                          
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200*----------------------------------------------------------------*        
081300 BACA-KONTR-FORTS SECTION.                                                
081400                                                                          
081500*----NYA NYCKLAR - KONTROLLERA  SAMBAND                                   
081600     IF MOD-IDARTNR-UT > ZERO AND                                         
081700        MOD-IDLEVNR-UT NOT = SPACE AND                                    
081800        MOD-IDFS-UT NOT = SPACE                                           
081900        CONTINUE                                                          
082000     ELSE                                                                 
082100*-FEL - 401                                                               
082200        MOVE NEJ                TO NYCKLAR-SW                             
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600*----------------------------------------------------------------*        
082700 BAD-KONTR-IDDC    SECTION.                                               
082800                                                                          
082900     IF MID-IDDC-IN = ALL '+' OR (NOT EGEN-MID)                           
083000        MOVE MSGI-IDDC      TO REQU-IDDC-KEY                              
083100     ELSE                                                                 
083200        MOVE MID-IDDC-IN    TO REQU-IDDC-KEY                              
083300        MOVE '7'            TO MFS-IDPFK                                  
083400     END-IF                                                               
083500                                                                          
083600     MOVE REQU-IDDC-KEY     TO MOD-IDDC-UT                                
083700     .                                                                    
083800     EJECT                                                                
083900*----------------------------------------------------------------*        
084000 BB-FORMELLA-KONTR SECTION.                                               
084100                                                                          
084200*----PARTINR                                                              
084300     IF MID-IDLOPNRM-UT > ZERO AND                                        
084400       (MID-IDARTNR-UT = ZERO OR                                          
084500        MID-IDARTNR-UT > ZERO) AND                                        
084600        MID-IDLEVNR-UT = SPACE AND                                        
084700        MID-IDFS-UT    = SPACE                                            
084800        PERFORM BBA-KONTR-PARTINR                                         
084900     ELSE                                                                 
085000*-------ARTNR                                                             
085100        IF MID-IDLOPNRM-UT = ZERO AND                                     
085200           MID-IDARTNR-UT > ZERO AND                                      
085300           MID-IDLEVNR-UT = SPACE AND                                     
085400           MID-IDFS-UT    = SPACE                                         
085500           PERFORM BBB-KONTR-ARTNR                                        
085600        ELSE                                                              
085700*----------ARTNR,LEVNR,FS                                                 
085800           IF (MID-IDLOPNRM-UT = ZERO) AND                                
085900              (MID-IDARTNR-UT > ZERO OR                                   
086000              MID-IDLEVNR-UT NOT = SPACE OR                               
086100              MID-IDFS-UT NOT = SPACE)                                    
086200              PERFORM BBC-KONTR-ART-LEV-FS                                
086300           ELSE                                                           
086400*-FEL - 401                                                               
086500              MOVE NEJ       TO NYCKLAR-SW                                
086600              MOVE '7'       TO MFS-IDPFK                                 
086700              MOVE SPACE     TO MFS-KDTRTYP                               
086800              IF MID-IDLOPNRM-UT  = ALL '+'                               
086900                 MOVE SPACE           TO MOD-IDLOPNRM-UT                  
087000                                         REQU-IDLOPNRM-KEY                
087100              ELSE                                                        
087200                 MOVE MID-IDLOPNRM-UT TO MOD-IDLOPNRM-UT                  
087300                                         REQU-IDLOPNRM-KEY                
087400              END-IF                                                      
087500              IF MID-IDARTNR-UT  = ALL '+'                                
087600                 MOVE SPACE          TO MOD-IDARTNR-UT                    
087700                                        REQU-IDARTNR-KEY                  
087800              ELSE                                                        
087900                 MOVE MID-IDARTNR-UT TO MOD-IDARTNR-UT                    
088000                                        REQU-IDARTNR-KEY                  
088100              END-IF                                                      
088200              IF MID-IDLEVNR-UT  = ALL '+'                                
088300                 MOVE SPACE          TO MOD-IDLEVNR-UT                    
088400                                        REQU-IDLEVNR-KEY                  
088500              ELSE                                                        
088600                 MOVE MID-IDLEVNR-UT TO MOD-IDLEVNR-UT                    
088700                                        REQU-IDLEVNR-KEY                  
088800              END-IF                                                      
088900              IF MID-IDFS-UT  = ALL '+'                                   
089000                 MOVE SPACE          TO MOD-IDFS-UT                       
089100                                        REQU-IDFS-KEY                     
089200              ELSE                                                        
089300                 MOVE MID-IDFS-UT    TO MOD-IDFS-UT                       
089400                                        REQU-IDFS-KEY                     
089500              END-IF                                                      
089600           END-IF                                                         
089700        END-IF                                                            
089800     END-IF                                                               
089900     .                                                                    
090000     EJECT                                                                
090100*----------------------------------------------------------------*        
090200 BBA-KONTR-PARTINR SECTION.                                               
090300                                                                          
090400     MOVE '1'                      TO NKLTYP-SW                           
090500     IF MID-IDLOPNRM-IN = ALL '+' AND                                     
090600        MID-IDLOPNRM-UT > ZERO                                            
090700*-------GML NKL - PARTINR                                                 
090800        MOVE MSGI-IDLOPNRM         TO MOD-IDLOPNRM-UT                     
090900                                      REQU-IDLOPNRM-KEY                   
091000        MOVE MID-IDARTNR-UT        TO MOD-IDARTNR-UT                      
091100        MOVE MSGI-IDDC             TO MOD-IDDC-UT                         
091200                                      REQU-IDDC-KEY                       
091300        MOVE SPACE                 TO REQU-IDARTNR-KEY                    
091400                                      REQU-IDLEVNR-KEY                    
091500                                      REQU-IDFS-KEY                       
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900*----------------------------------------------------------------*        
092000 BBB-KONTR-ARTNR SECTION.                                                 
092100                                                                          
092200     MOVE '2'                      TO NKLTYP-SW                           
092300     IF MID-IDARTNR-IN = ALL '+' AND                                      
092400        MID-IDARTNR-UT > ZERO                                             
092500*-------GML NKL - ARTNR                                                   
092600        MOVE MSGI-IDARTNR          TO MOD-IDARTNR-UT                      
092700                                      REQU-IDARTNR-KEY                    
092800        MOVE MSGI-IDDC             TO MOD-IDDC-UT                         
092900                                      REQU-IDDC-KEY                       
093000        MOVE SPACE                 TO REQU-IDLOPNRM-KEY                   
093100                                      REQU-IDLEVNR-KEY                    
093200                                      REQU-IDFS-KEY                       
093300     END-IF                                                               
093400     .                                                                    
093500     EJECT                                                                
093600*----------------------------------------------------------------*        
093700 BBC-KONTR-ART-LEV-FS SECTION.                                            
093800                                                                          
093900     MOVE '3'                     TO NKLTYP-SW                            
094000     MOVE MID-IDDC-UT             TO MOD-IDDC-UT                          
094100                                     REQU-IDDC-KEY                        
094200     MOVE ALL '+'                 TO REQU-IDLOPNRM-KEY                    
094300                                                                          
094400     IF MID-IDARTNR-IN = ALL '+' AND                                      
094500        MID-IDARTNR-UT > ZERO                                             
094600*-------GML NKL - ARTNR                                                   
094700        MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                          
094800                                  REQU-IDARTNR-KEY                        
094900     END-IF                                                               
095000                                                                          
095100     IF MID-IDLEVNR-IN = ALL '+' AND                                      
095200        MID-IDLEVNR-UT NOT = SPACE                                        
095300*------GML NKL - LEVNR                                                    
095400        MOVE MSGI-IDLEVNR       TO MOD-IDLEVNR-UT                         
095500                                   REQU-IDLEVNR-KEY                       
095600     END-IF                                                               
095700                                                                          
095800     IF MID-IDFS-IN = ALL '+' AND                                         
095900        MID-IDFS-UT NOT = SPACE                                           
096000*------GML NKL - FS                                                       
096100        MOVE MSGI-IDFS          TO MOD-IDFS-UT                            
096200                                   REQU-IDFS-KEY                          
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 BC-KOLLA-ARTNR-HOPP SECTION.                                             
096700                                                                          
096800     MOVE MSGI-IDARTNR  TO MOD-IDARTNR-UT                                 
096900                           REQU-IDARTNR-KEY                               
097000                                                                          
097100     INSPECT MOD-IDARTNR-UT REPLACING LEADING SPACE BY ZERO               
097200     IF MOD-IDARTNR-UT NUMERIC AND MOD-IDARTNR-UT > +0                    
097300       MOVE JA TO NYCKLAR-SW                                              
097400       MOVE '2' TO NKLTYP-SW                                              
097500     ELSE                                                                 
097600       MOVE NEJ TO ARTNR-SW                                               
097700     END-IF                                                               
097800     .                                                                    
097900     EJECT                                                                
098000*----------------------------------------------------------------*        
098100 D-NAESTA-SIDA SECTION.                                                   
098200                                                                          
098300     IF NKLTYP1                                                           
098400       IF MSGI-IDTRANS = '6123'                                           
098500*----------FEL DETTA ÄR SISTA SIDAN                                       
098600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
098700          PERFORM MFS-LAES-IN-IGEN                                        
098800       ELSE                                                               
098900          PERFORM MFS-ROER-EJ-FAELT-UT                                    
099000       END-IF                                                             
099100     END-IF                                                               
099200                                                                          
099300     IF NKLTYP2                                                           
099400       IF MSGI-IDTRANS = '6123'                                           
099500*----------FEL DETTA ÄR SISTA SIDAN                                       
099600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
099700       END-IF                                                             
099800     END-IF                                                               
099900                                                                          
100000     IF NKLTYP3                                                           
100100       IF MSGI-IDTRANS = '6123'                                           
100200*----------FEL DETTA ÄR SISTA SIDAN                                       
100300          PERFORM MFS-ROER-EJ-FAELT-UT                                    
100400          PERFORM MFS-LAES-IN-IGEN                                        
100500       ELSE                                                               
100600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
100700       END-IF                                                             
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100*----------------------------------------------------------------*        
101200 E-SAMMA-SIDA SECTION.                                                    
101300                                                                          
101400*----BLANKA/NOLLA UT BLÄDDRINGSNYCKLAR                                    
101500     PERFORM MFS-RENSA-FAELT-UT                                           
101600     IF HELP-MID                                                          
101700        CONTINUE                                                          
101800     ELSE                                                                 
101900        IF (MID-KDCMDVAL-INPUT(1) = ALL '+' OR                            
102000           MID-KDCMDVAL-INPUT(1) = SPACE) AND                             
102100           (MID-KDCMDVAL-INPUT(2) = ALL '+' OR                            
102200           MID-KDCMDVAL-INPUT(2) = SPACE) AND                             
102300           (MID-KDCMDVAL-INPUT(3) = ALL '+' OR                            
102400           MID-KDCMDVAL-INPUT(3) = SPACE) AND                             
102500           (MID-KDCMDVAL-INPUT(4) = ALL '+' OR                            
102600           MID-KDCMDVAL-INPUT(4) = SPACE) AND                             
102700           (MID-KDCMDVAL-INPUT(5) = ALL '+' OR                            
102800           MID-KDCMDVAL-INPUT(5) = SPACE) AND                             
102900           (MID-KDCMDVAL-INPUT(6) = ALL '+' OR                            
103000           MID-KDCMDVAL-INPUT(6) = SPACE) AND                             
103100           (MID-KDCMDVAL-INPUT(7) = ALL '+' OR                            
103200           MID-KDCMDVAL-INPUT(7) = SPACE) AND                             
103300           (MID-KDCMDVAL-INPUT(8) = ALL '+' OR                            
103400           MID-KDCMDVAL-INPUT(8) = SPACE) AND                             
103500           (MID-KDCMDVAL-INPUT(9) = ALL '+' OR                            
103600           MID-KDCMDVAL-INPUT(9) = SPACE) AND                             
103700           (MID-KDCMDVAL-INPUT(10) = ALL '+' OR                           
103800           MID-KDCMDVAL-INPUT(10) = SPACE) AND                            
103900           (MID-KDCMDVAL-INPUT(11) = ALL '+' OR                           
104000           MID-KDCMDVAL-INPUT(11) = SPACE)                                
104100           CONTINUE                                                       
104200        ELSE                                                              
104300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
104400           PERFORM MFS-LAES-IN-IGEN                                       
104500        END-IF                                                            
104600     END-IF                                                               
104700     .                                                                    
104800     EJECT                                                                
104900*----------------------------------------------------------------*        
105000 F-CALL-BIZ-LOGIC-W6012310 SECTION.                                       
105100                                                                          
105200     CALL W6012310 USING REQU-AREA RESP-AREA MAX-KVRADER                  
105300                         ALT1-PCB ALT2-PCB ALT3-PCB USEA-PCB              
105400                         INLA-PCB INLB1-PCB INLB-PCB INLH1-PCB            
105500                         LOPA-PCB PLAA-PCB INLB-PMRK-PCB                  
105600                         INLC-PMRK-PCB WDB6-PCB BENA-PCB                  
105700                                                                          
105800     PERFORM FA-SET-MSG-AND-HILIGHT                                       
105900     IF NOT WRONG-KEY                                                     
106000       PERFORM FB-MOVE-RESP-TO-MOD                                        
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400*----------------------------------------------------------------*        
106500 FA-SET-MSG-AND-HILIGHT SECTION.                                          
106600                                                                          
106700     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
106800                                                                          
106900     IF WRONG-KEY                                                         
107000       PERFORM MFS-RENSA-FAELT-IN                                         
107100       PERFORM MFS-RENSA-FAELT-UT                                         
107200     END-IF                                                               
107300                                                                          
107400     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
107500     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
107600     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
107700                                                                          
107800     CALL WL01MCNV USING MCNV-AREA                                        
107900                                                                          
108000     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
108100     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
108200     .                                                                    
108300     EJECT                                                                
108400*----------------------------------------------------------------*        
108500 FB-MOVE-RESP-TO-MOD SECTION.                                             
108600                                                                          
108700     IF RESP-IDLOPNRM-KEY = SPACE OR                                      
108800        RESP-IDLOPNRM-KEY = ALL '+'                                       
108900        IF MOD-IDLOPNRM-UT = SPACE OR                                     
109000           MOD-IDLOPNRM-UT = ALL '+'                                      
109100           MOVE MFS-RENSA-FAELT     TO MOD-IDLOPNRM-UT                    
109200        END-IF                                                            
109300     ELSE                                                                 
109400        MOVE RESP-IDLOPNRM-KEY      TO MOD-IDLOPNRM-UT                    
109500     END-IF                                                               
109600                                                                          
109700     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
109800     IF RESP-IDARTNR-KEY = SPACE OR                                       
109900        RESP-IDARTNR-KEY = ALL '+'                                        
110000        IF MOD-IDARTNR-UT = SPACE OR                                      
110100           MOD-IDARTNR-UT = ALL '+'                                       
110200           MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-UT                     
110300        END-IF                                                            
110400     ELSE                                                                 
110500        MOVE RESP-IDARTNR-KEY      TO MOD-IDARTNR-UT                      
110600     END-IF                                                               
110700                                                                          
110800     IF RESP-IDLEVNR-KEY = SPACE OR                                       
110900        RESP-IDLEVNR-KEY = ALL '+'                                        
111000        IF MOD-IDLEVNR-UT = SPACE OR                                      
111100           MOD-IDLEVNR-UT = ALL '+'                                       
111200           MOVE MFS-RENSA-FAELT     TO MOD-IDLEVNR-UT                     
111300        END-IF                                                            
111400     ELSE                                                                 
111500        MOVE RESP-IDLEVNR-KEY       TO MOD-IDLEVNR-UT                     
111600     END-IF                                                               
111700                                                                          
111800     IF RESP-IDFS-KEY = SPACE OR                                          
111900        RESP-IDFS-KEY = ALL '+'                                           
112000        IF MOD-IDFS-UT = SPACE OR                                         
112100           MOD-IDFS-UT = ALL '+'                                          
112200           MOVE MFS-RENSA-FAELT     TO MOD-IDFS-UT                        
112300        END-IF                                                            
112400     ELSE                                                                 
112500        MOVE RESP-IDFS-KEY          TO MOD-IDFS-UT                        
112600     END-IF                                                               
112700                                                                          
112800     IF RESP-IDLBBET-KEY = SPACE OR                                       
112900        RESP-IDLBBET-KEY = ALL '+'                                        
113000        IF MOD-IDLBBET-UT = SPACE OR                                      
113100           MOD-IDLBBET-UT = ALL '+'                                       
113200           MOVE MFS-RENSA-FAELT     TO MOD-IDLBBET-UT                     
113300        END-IF                                                            
113400     ELSE                                                                 
113500        MOVE RESP-IDLBBET-KEY       TO MOD-IDLBBET-UT                     
113600     END-IF                                                               
113700                                                                          
113800     IF RESP-IDDC-KEY = SPACE OR                                          
113900        RESP-IDDC-KEY = ALL '+'                                           
114000        IF MOD-IDDC-UT = SPACE OR                                         
114100           MOD-IDDC-UT = ALL '+'                                          
114200           MOVE MFS-RENSA-FAELT     TO MOD-IDDC-UT                        
114300        END-IF                                                            
114400     ELSE                                                                 
114500        MOVE RESP-IDDC-KEY          TO MOD-IDDC-UT                        
114600     END-IF                                                               
114700                                                                          
114800     IF RESP-ADINLOMR-PRT = SPACE OR                                      
114900        RESP-ADINLOMR-PRT = ALL '+'                                       
115000        IF MOD-ADINLOMR-PRT = SPACE                                       
115100           MOVE MFS-RENSA-FAELT     TO MOD-ADINLOMR-PRT                   
115200        ELSE                                                              
115300           MOVE MFS-ROER-EJ-FAELT   TO MOD-ADINLOMR-PRT                   
115400        END-IF                                                            
115500     ELSE                                                                 
115600        MOVE RESP-ADINLOMR-PRT      TO MOD-ADINLOMR-PRT                   
115700        MOVE RESP-ADINLOMR-PRT-ATTR TO MOD-ADINLOMR-PRT-ATTR              
115800     END-IF                                                               
115900                                                                          
116000     IF RESP-ADGANG = SPACE OR                                            
116100        RESP-ADGANG = ALL '+'                                             
116200        MOVE MFS-RENSA-FAELT        TO MOD-ADGANG                         
116300        IF RESP-ADGANG = ALL '+'                                          
116400           MOVE MFS-ROER-EJ-FAELT   TO MOD-ADGANG                         
116500        END-IF                                                            
116600     ELSE                                                                 
116700        MOVE RESP-ADGANG            TO MOD-ADGANG                         
116800     END-IF                                                               
116900                                                                          
117000     IF RESP-ADLAGOMR = SPACE OR                                          
117100        RESP-ADLAGOMR = ALL '+'                                           
117200        MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR                       
117300        IF RESP-ADLAGOMR = ALL '+'                                        
117400           MOVE MFS-ROER-EJ-FAELT   TO MOD-ADLAGOMR                       
117500        END-IF                                                            
117600     ELSE                                                                 
117700        MOVE RESP-ADLAGOMR          TO MOD-ADLAGOMR                       
117800     END-IF                                                               
117900                                                                          
118000     IF RESP-ADPLATS = SPACE OR                                           
118100        RESP-ADPLATS = ALL '+'                                            
118200        MOVE MFS-RENSA-FAELT        TO MOD-ADPLATS                        
118300        IF RESP-ADPLATS = ALL '+'                                         
118400           MOVE MFS-ROER-EJ-FAELT   TO MOD-ADPLATS                        
118500        END-IF                                                            
118600     ELSE                                                                 
118700        MOVE RESP-ADPLATS           TO MOD-ADPLATS                        
118800     END-IF                                                               
118900                                                                          
119000     IF RESP-KDLAGEMB = SPACE OR                                          
119100        RESP-KDLAGEMB = ALL '+'                                           
119200        MOVE MFS-RENSA-FAELT        TO MOD-KDLAGEMB                       
119300        IF RESP-KDLAGEMB = ALL '+'                                        
119400           MOVE MFS-ROER-EJ-FAELT   TO MOD-KDLAGEMB                       
119500        END-IF                                                            
119600     ELSE                                                                 
119700        MOVE RESP-KDLAGEMB          TO MOD-KDLAGEMB                       
119800     END-IF                                                               
119900                                                                          
120000     IF RESP-BEART = SPACE OR                                             
120100        RESP-BEART = ALL '+'                                              
120200        MOVE MFS-RENSA-FAELT        TO MOD-BEART                          
120300        IF RESP-BEART = ALL '+'                                           
120400           MOVE MFS-ROER-EJ-FAELT   TO MOD-BEART                          
120500        END-IF                                                            
120600     ELSE                                                                 
120700        MOVE RESP-BEART             TO MOD-BEART                          
120800     END-IF                                                               
120900                                                                          
121000     IF RESP-KDSORT = SPACE OR                                            
121100        RESP-KDSORT = ALL '+'                                             
121200        MOVE MFS-RENSA-FAELT        TO MOD-KDSORT                         
121300        IF RESP-KDSORT = ALL '+'                                          
121400           MOVE MFS-ROER-EJ-FAELT   TO MOD-KDSORT                         
121500        END-IF                                                            
121600     ELSE                                                                 
121700        MOVE RESP-KDSORT            TO MOD-KDSORT                         
121800     END-IF                                                               
121900                                                                          
122000     IF RESP-KDFARLIG-TXT = SPACE OR                                      
122100        RESP-KDFARLIG-TXT = ALL '+'                                       
122200        MOVE MFS-RENSA-FAELT        TO MOD-KDFARLIG-TXT                   
122300        IF RESP-KDFARLIG-TXT = ALL '+'                                    
122400           MOVE MFS-ROER-EJ-FAELT   TO MOD-KDFARLIG-TXT                   
122500        END-IF                                                            
122600     ELSE                                                                 
122700        MOVE RESP-KDFARLIG-TXT      TO MOD-KDFARLIG-TXT                   
122800     END-IF                                                               
122900                                                                          
123000     IF RESP-BEPRTLST = ALL '+'                                           
123100        CONTINUE                                                          
123200     ELSE                                                                 
123300        MOVE RESP-BEPRTLST          TO MOD-TEMFSFEL                       
123400     END-IF                                                               
123500                                                                          
123600     MOVE 1  TO IX                                                        
123700     MOVE RESP-KVRADER TO OUT-KVRADER                                     
123800     PERFORM UNTIL IX > OUT-KVRADER                                       
123900        INSPECT RESP-IDRADNR-LINE(IX)                                     
124000                                  REPLACING LEADING ZERO BY SPACE         
124100        INSPECT RESP-IDLOPNRM-LINE(IX)                                    
124200                                  REPLACING LEADING ZERO BY SPACE         
124300        INSPECT RESP-IDOKOLLI-LINE(IX)                                    
124400                                  REPLACING LEADING ZERO BY SPACE         
124500        INSPECT RESP-IDINLVGN-LINE(IX)                                    
124600                                  REPLACING LEADING ZERO BY SPACE         
124900        INSPECT RESP-KVINLART-LINE(IX)                                    
125000                                  REPLACING LEADING ZERO BY SPACE         
125100        INSPECT RESP-ADINLOMR-LINE(IX)                                    
125200                                  REPLACING LEADING ZERO BY SPACE         
125300        INSPECT RESP-ADINLOMR-NXT-LINE(IX)                                
125400                                  REPLACING LEADING ZERO BY SPACE         
125500        INSPECT RESP-KDKLIPRI-LINE(IX)                                    
125600                                  REPLACING LEADING ZERO BY SPACE         
125700        INSPECT RESP-FLSATS-LINE(IX)                                      
125800                                  REPLACING LEADING ZERO BY SPACE         
125900                                                                          
126000        IF RESP-KDCMDVAL-INPUT-LINE(IX) NOT = SPACE AND                   
126100           RESP-KDCMDVAL-INPUT-LINE(IX) NOT = ALL '+'                     
126200           MOVE RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                         
126300                                  TO MOD-KDCMDVAL-INPUT-ATTR(IX)          
126400           MOVE RESP-KDCMDVAL-INPUT-LINE(IX)                              
126500                                       TO MOD-KDCMDVAL-INPUT(IX)          
126600        END-IF                                                            
126700                                                                          
126800        IF RESP-IDRADNR-LINE(IX) = SPACE                                  
126900           MOVE MFS-RENSA-FAELT        TO MOD-IDRADNR(IX)                 
127000                                          MOD-KDINLSTA(IX)                
127100        ELSE                                                              
127200           IF RESP-IDRADNR-LINE(IX) = ALL '+'                             
127300              MOVE MFS-ROER-EJ-FAELT   TO MOD-IDRADNR(IX)                 
127400                                          MOD-KDINLSTA(IX)                
127500           ELSE                                                           
127600              MOVE RESP-IDRADNR-LINE(IX) TO MOD-IDRADNR(IX)               
127700              MOVE RESP-KDINLSTA-LINE(IX) TO MOD-KDINLSTA(IX)             
127800              MOVE RESP-KDINLSTA-LINE-ATTR(IX)                            
127900                                       TO MOD-KDINLSTA-ATTR(IX)           
128000           END-IF                                                         
128100        END-IF                                                            
128200                                                                          
128300        IF RESP-IDLOPNRM-LINE(IX) = SPACE                                 
128400           MOVE MFS-RENSA-FAELT        TO MOD-IDLOPNRM(IX)                
128500        ELSE                                                              
128600           IF RESP-IDLOPNRM-LINE(IX) = ALL '+'                            
128700              MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLOPNRM(IX)                
128800           ELSE                                                           
128900              MOVE RESP-IDLOPNRM-LINE(IX) TO MOD-IDLOPNRM(IX)             
129000           END-IF                                                         
129100        END-IF                                                            
129200                                                                          
129300        IF RESP-KVINLART-LINE(IX) = SPACE                                 
129400           MOVE MFS-RENSA-FAELT        TO MOD-KVINLART(IX)                
129500        ELSE                                                              
129600           IF RESP-KVINLART-LINE(IX) = ALL '+'                            
129700              MOVE MFS-ROER-EJ-FAELT   TO MOD-KVINLART(IX)                
129800           ELSE                                                           
129900              MOVE RESP-KVINLART-LINE(IX) TO MOD-KVINLART(IX)             
130000           END-IF                                                         
130100        END-IF                                                            
130200                                                                          
130300        IF RESP-ADINLOMR-LINE(IX) = SPACE                                 
130400           MOVE MFS-RENSA-FAELT        TO MOD-ADINLOMR(IX)                
130500        ELSE                                                              
130600           IF RESP-ADINLOMR-LINE(IX) = ALL '+'                            
130700              MOVE MFS-ROER-EJ-FAELT   TO MOD-ADINLOMR(IX)                
130800           ELSE                                                           
130900              MOVE RESP-ADINLOMR-LINE(IX) TO MOD-ADINLOMR(IX)             
131000           END-IF                                                         
131100        END-IF                                                            
131200                                                                          
131300        IF RESP-ADINLOMR-NXT-LINE(IX) = SPACE                             
131400           MOVE MFS-RENSA-FAELT        TO MOD-ADINLOMR-NXT(IX)            
131500        ELSE                                                              
131600           IF RESP-ADINLOMR-NXT-LINE(IX) = ALL '+'                        
131700              MOVE MFS-ROER-EJ-FAELT   TO MOD-ADINLOMR-NXT(IX)            
131800           ELSE                                                           
131900              MOVE RESP-ADINLOMR-NXT-LINE(IX)                             
132000                                       TO MOD-ADINLOMR-NXT(IX)            
132100           END-IF                                                         
132200        END-IF                                                            
132300                                                                          
132400        IF RESP-IDLEVNR-LINE(IX) = SPACE                                  
132500           MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR(IX)                 
132600        ELSE                                                              
132700           IF RESP-IDLEVNR-LINE(IX) = ALL '+'                             
132800              MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLEVNR(IX)                 
132900           ELSE                                                           
133000              MOVE RESP-IDLEVNR-LINE(IX) TO MOD-IDLEVNR(IX)               
133100           END-IF                                                         
133200        END-IF                                                            
133300                                                                          
133400        IF RESP-IDOKOLLI-LINE(IX) = SPACE                                 
133500           MOVE MFS-RENSA-FAELT        TO MOD-IDOKOLLI(IX)                
133600        ELSE                                                              
133700           IF RESP-IDOKOLLI-LINE(IX) = ALL '+'                            
133800              MOVE MFS-ROER-EJ-FAELT   TO MOD-IDOKOLLI(IX)                
133900           ELSE                                                           
134000              MOVE RESP-IDOKOLLI-LINE(IX) TO MOD-IDOKOLLI(IX)             
134100           END-IF                                                         
134200        END-IF                                                            
134300                                                                          
134400        IF RESP-IDINLVGN-LINE(IX) = SPACE                                 
134500           MOVE MFS-RENSA-FAELT        TO MOD-IDINLVGN(IX)                
134600        ELSE                                                              
134700           IF RESP-IDINLVGN-LINE(IX) = ALL '+'                            
134800              MOVE MFS-ROER-EJ-FAELT   TO MOD-IDINLVGN(IX)                
134900           ELSE                                                           
135000              MOVE RESP-IDINLVGN-LINE(IX) TO MOD-IDINLVGN(IX)             
135100           END-IF                                                         
135200        END-IF                                                            
135300                                                                          
135400        IF RESP-KDKLIPRI-LINE(IX) = SPACE                                 
135500           MOVE MFS-RENSA-FAELT        TO MOD-KDKLIPRI(IX)                
135600        ELSE                                                              
135700           IF RESP-KDKLIPRI-LINE(IX) = ALL '+'                            
135800              MOVE MFS-ROER-EJ-FAELT   TO MOD-KDKLIPRI(IX)                
135900           ELSE                                                           
136000              MOVE RESP-KDKLIPRI-LINE(IX) TO MOD-KDKLIPRI(IX)             
136100              MOVE RESP-KDKLIPRI-LINE-ATTR(IX)                            
136200                                       TO MOD-KDKLIPRI-ATTR(IX)           
136300           END-IF                                                         
136400        END-IF                                                            
136500                                                                          
136600        IF RESP-FLSATS-LINE(IX) = SPACE                                   
136700           MOVE MFS-RENSA-FAELT        TO MOD-FLSATS(IX)                  
136800        ELSE                                                              
136900           IF RESP-FLSATS-LINE(IX) = ALL '+'                              
137000              MOVE MFS-ROER-EJ-FAELT   TO MOD-FLSATS(IX)                  
137100           ELSE                                                           
137200              MOVE RESP-FLSATS-LINE(IX) TO MOD-FLSATS(IX)                 
137300              MOVE RESP-FLSATS-LINE-ATTR(IX)                              
137400                                       TO MOD-FLSATS-ATTR(IX)             
137500           END-IF                                                         
137600        END-IF                                                            
137700                                                                          
137800        ADD 1 TO IX                                                       
137900     END-PERFORM                                                          
138000                                                                          
138100     PERFORM UNTIL IX > MAX-KVRADER                                       
138200        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
138300        ADD +1 TO IX                                                      
138400     END-PERFORM                                                          
138500     .                                                                    
138600     EJECT                                                                
138700******************************************************************        
138800*    MFS-REDIGERING AV BILDENS FÄLT                              *        
138900******************************************************************        
139000*----------------------------------------------------------------*        
139100 MFS-RENSA-FAELT-UT SECTION.                                              
139200                                                                          
139300*    --- ALLA UTDATA-FÄLT                                                 
139400*    --- INKL. BLÄDDRINGSNYCKLAR - SPAR-FÄLT                              
139500                                                                          
139600     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
139700                                MOD-KDLAGEMB                              
139800                                MOD-ADLAGOMR                              
139900                                MOD-ADGANG                                
140000                                MOD-ADPLATS                               
140100                                MOD-KDSORT                                
140200                                MOD-KDFARLIG-TXT                          
140300                                                                          
140400*--- RENSA INDEXERADE RADER                                               
140500                                                                          
140600     MOVE +1 TO IX                                                        
140700     PERFORM UNTIL IX > MAX-KVRADER                                       
140800        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
140900        ADD +1 TO IX                                                      
141000     END-PERFORM                                                          
141100     .                                                                    
141200     SKIP2                                                                
141300     EJECT                                                                
141400*----------------------------------------------------------------*        
141500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
141600                                                                          
141700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
141800                                                                          
141900     MOVE MFS-RENSA-FAELT    TO MOD-IDLOPNRM(IX)                          
142000                                MOD-KDCMDVAL-INPUT(IX)                    
142100                                MOD-IDRADNR(IX)                           
142200                                MOD-IDLEVNR(IX)                           
142300                                MOD-IDOKOLLI(IX)                          
142400                                MOD-KVINLART(IX)                          
142500                                MOD-ADINLOMR(IX)                          
142600                                MOD-IDINLVGN(IX)                          
142700                                MOD-ADINLOMR-NXT(IX)                      
142800                                MOD-KDKLIPRI(IX)                          
142900                                MOD-FLSATS(IX)                            
143000                                MOD-KDINLSTA(IX)                          
143100     .                                                                    
143200     EJECT                                                                
143300     SKIP2                                                                
143400*----------------------------------------------------------------*        
143500 MFS-RENSA-FAELT-IN SECTION.                                              
143600                                                                          
143700*    --- ALLA INDATA-FÄLT                                                 
143800     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
143900                             MOD-IDARTNR-IN                               
144000                             MOD-IDLEVNR-IN                               
144100                             MOD-IDFS-IN                                  
144200                             MOD-IDLBBET-IN                               
144300                             MOD-IDDC-IN                                  
144400     .                                                                    
144500     EJECT                                                                
144600     SKIP2                                                                
144700*----------------------------------------------------------------*        
144800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
144900                                                                          
145000*    --- ALLA UTDATA-FÄLT                                                 
145100*    --- INKL BLÄDDRINGSNYCKLAR                                           
145200                                                                          
145300     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                                 
145400                                MOD-KDLAGEMB                              
145500                                MOD-ADLAGOMR                              
145600                                MOD-ADGANG                                
145700                                MOD-ADPLATS                               
145800                                MOD-KDSORT                                
145900                                MOD-KDFARLIG-TXT                          
146000                                MOD-ADINLOMR-PRT                          
146100                                                                          
146200                                                                          
146300*--- RÖREJ INDEXERADE RADER                                               
146400                                                                          
146500     MOVE +1 TO IX                                                        
146600     PERFORM UNTIL IX > MAX-KVRADER                                       
146700                                                                          
146800        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM(IX)                        
146900                                  MOD-KDCMDVAL-INPUT(IX)                  
147000                                  MOD-IDRADNR(IX)                         
147100                                  MOD-IDLEVNR(IX)                         
147200                                  MOD-IDOKOLLI(IX)                        
147300                                  MOD-KVINLART(IX)                        
147400                                  MOD-ADINLOMR(IX)                        
147500                                  MOD-IDINLVGN(IX)                        
147600                                  MOD-ADINLOMR-NXT(IX)                    
147700                                  MOD-KDKLIPRI(IX)                        
147800                                  MOD-FLSATS(IX)                          
147900                                  MOD-KDINLSTA(IX)                        
148000        ADD +1 TO IX                                                      
148100     END-PERFORM                                                          
148200     .                                                                    
148300     EJECT                                                                
148400     SKIP2                                                                
148500*----------------------------------------------------------------*        
148600 MFS-LAES-IN-IGEN SECTION.                                                
148700                                                                          
148800*    INDATA-FÄLT                                                          
148900                                                                          
149000     MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-ADINLOMR-PRT-ATTR               
149100                                                                          
149200*--- RENSA INDEXERADE RADER                                               
149300                                                                          
149400     MOVE +1 TO IX                                                        
149500     PERFORM UNTIL IX > MAX-KVRADER                                       
149600                                                                          
149700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-INPUT-ATTR(IX)         
149800                                      MOD-KDKLIPRI-ATTR(IX)               
149900                                      MOD-FLSATS-ATTR(IX)                 
150000                                      MOD-KDINLSTA-ATTR(IX)               
150100        ADD +1 TO IX                                                      
150200     END-PERFORM                                                          
150300                                                                          
150400     .                                                                    
150500     EJECT                                                                
150600******************************************************************        
150700*  IMS-SECTIONER                                                 *        
150800******************************************************************        
150900     SKIP3                                                                
151000*----------------------------------------------------------------*        
151100 IMS-GET-MSG SECTION.                                                     
151200                                                                          
151300     MOVE '  QC' TO GODK-STATUSKODER                                      
151400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
151500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
151600     PERFORM IMS-STATUSKONTROLL                                           
151700     .                                                                    
151800     SKIP3                                                                
151900*----------------------------------------------------------------*        
152000 IMS-INSERT-MSG SECTION.                                                  
152100                                                                          
152200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
152300       MOVE '0' TO MFS-KDHUVOMR                                           
152400     END-IF                                                               
152500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152600     MOVE SPACE TO GODK-STATUSKODER                                       
152700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
152800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153200     SKIP3                                                                
153300                                                                          
153400*----------------------------------------------------------------*        
153500 IMS-STATUSKONTROLL SECTION.                                              
153600                                                                          
153700     SET STATUS-IX TO 1                                                   
153800     SEARCH GODK-STATUS                                                   
153900       AT END                                                             
154000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
154100         DELIMITED BY SIZE INTO FELTEXT                                   
154200         CALL FELLOG                                                      
154300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
154400         CONTINUE                                                         
154500     END-SEARCH                                                           
154600     .                                                                    
154700     EJECT                                                                
154800     SKIP3                                                                
154900******************************************************************        
155000*  BILD-REDIGERING                                               *        
155100******************************************************************        
155200*----------------------------------------------------------------*        
155300 S90-BLANKUTF-NUM-FAELT SECTION.                                          
155400                                                                          
155500*----NKL-FÄLT                                                             
155600     INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE              
155700     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
155800*----HUV-FÄLT                                                             
155900     INSPECT MOD-ADLAGOMR    REPLACING LEADING ZERO BY SPACE              
156000     INSPECT MOD-ADGANG      REPLACING LEADING ZERO BY SPACE              
156100     INSPECT MOD-ADPLATS     REPLACING LEADING ZERO BY SPACE              
156200                                                                          
156300*----RAD-FÄLT                                                             
156400*--- INDEXERADE RADER                                                     
156500                                                                          
156600     MOVE +1 TO IX                                                        
156700     PERFORM UNTIL IX > MAX-KVRADER                                       
156800        INSPECT MOD-IDLOPNRM(IX) REPLACING LEADING ZERO BY SPACE          
156900        INSPECT MOD-IDRADNR(IX)  REPLACING LEADING ZERO BY SPACE          
157000        INSPECT MOD-IDOKOLLI(IX) REPLACING LEADING ZERO BY SPACE          
157100        INSPECT MOD-IDINLVGN(IX) REPLACING LEADING ZERO BY SPACE          
157200                                                                          
157300        IF MOD-KDKLIPRI(IX) = 'P'                                         
157400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDKLIPRI-ATTR(IX)            
157500        END-IF                                                            
157600        IF INDATA-OK                                                      
157700           IF MOD-IDRADNR(IX) = SPACE                                     
157800              MOVE MFS-STAENG-FAELT                                       
157900                   TO MOD-KDCMDVAL-INPUT-ATTR(IX)                         
158000           ELSE                                                           
158100              IF MOD-KDINLSTA(IX) = ('FPK' OR 'SAK' OR SPACE OR           
158200                                     'PP ' OR 'MIS')                      
158300                 IF MOD-KDCMDVAL-INPUT(IX) = SPACE OR ALL '+'             
158400                    MOVE MFS-OEPPNA-ALFA-FAELT                            
158500                         TO MOD-KDCMDVAL-INPUT-ATTR(IX)                   
158600                 END-IF                                                   
158700              ELSE                                                        
158800                 MOVE MFS-STAENG-FAELT                                    
158900                      TO MOD-KDCMDVAL-INPUT-ATTR(IX)                      
159000              END-IF                                                      
159100           END-IF                                                         
159200        END-IF                                                            
159300                                                                          
159400*--------------------------------------------------------------*          
159500*----DENNA DEL GÖRS FÖR ATT FÅ ORDNING PÅ ÖPPNA/STÄNGDA FÄLT---*          
159600*--------------------------------------------------------------*          
159700        IF STATUS-JA(IX) = NEJ                                            
159800           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDINLSTA-ATTR(IX)            
159900        END-IF                                                            
160000*--------------------------------------------------------------*          
160100        ADD +1 TO IX                                                      
160200     END-PERFORM                                                          
160300     .                                                                    
