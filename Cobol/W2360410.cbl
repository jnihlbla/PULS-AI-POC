000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W2360410.                                    
000400*AUTHOR.                     THOMAS FALLENIUS.                            
000500*DATE-WRITTEN.               DEC 1979.                                    
000600*REMARKS.                                                                 
000700*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2360400                       
000800*        SOM SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA.                     
000900     SKIP3                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100     SKIP3                                                                
001200 DATA DIVISION.                                                           
001300     EJECT                                                                
001400 WORKING-STORAGE SECTION.                                                 
001500*    -COPY WY2000W1                                                       
001600     SKIP3                                                                
001700 77      FELTEXT         PIC X(80)   VALUE SPACE.                         
001800 77      SAKNAS          PIC X       VALUE 'S'.                           
001900 77      FINNS           PIC X       VALUE ' '.                           
001901                                                                          
001910*01  -COPY WWDCKONS                                                       
001920                                                                          
002000     SKIP3                                                                
002100 01  ARBETSFAELT.                                                         
002200     03  WS-DAAVROP-AVS              PIC 9(6).                            
002300     03  FILLER  REDEFINES WS-DAAVROP-AVS.                                
002400         05  WS-DAAVROP-SS           PIC 9(2).                            
002500         05  WS-DAAVROP-AAVV         PIC 9(4).                            
002600     03  WS-DALEVBSK-AVS             PIC 9(8).                            
002700     03  FILLER  REDEFINES WS-DALEVBSK-AVS.                               
002800         05  WS-DALEVBSK-SS          PIC 9(2).                            
002900         05  WS-DALEVBSK-AAMMDD      PIC 9(6).                            
003000                                                                          
003100   03  WS-TIAAVVD.                                                        
003200     05  WS-TIAAVV          PIC 9(4)   VALUE ZERO.                        
003300     05  FILLER             PIC 9(1)   VALUE ZERO.                        
003400                                                                          
003500*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
003600  02     WS-IDAVINR              PIC 9(7)    VALUE ZERO.                  
003700  02     FILLER                  REDEFINES WS-IDAVINR.                    
003800   03    WS-IDAVINR-TKN          OCCURS 7                                 
003900                                 PIC 9(1).                                
004000*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
004100  02     WS-IDFS                 PIC X(8)    VALUE SPACE.                 
004200  02     FILLER                  REDEFINES WS-IDFS.                       
004300   03    WS-IDFS-TKN             OCCURS 8                                 
004400                                 PIC 9(1).                                
004500*     -- FÄLTLÄNGD IDFS                                                   
004600  02     K-IDFS-LNG              PIC S9(9)   VALUE +8   COMP SYNC.        
004700*     -- FÄLTLÄNGD IDAVINR                                                
004800  02     K-IDAVINR-LNG           PIC S9(9)   VALUE +7   COMP SYNC.        
004900                                                                          
005000 01      IX-INDEXVARIABLER.                                               
005100                                                                          
005200*     -- TECKEN I WS-IDFS                                                 
005300  02     IX-IDFS                 PIC S9(9)   VALUE ZERO COMP SYNC.        
005400*     -- TECKEN I WS-IDAVINR                                              
005500  02     IX-IDAVINR              PIC S9(9)   VALUE ZERO COMP SYNC.        
005600                                                                          
005700     SKIP3                                                                
005800 01  KONSTANTER.                                                          
005900                                                                          
006000   03  LAS-ART-INFO         PIC S9(3)   VALUE +101      COMP-3.           
006100   03  LAS-LEV-INFO         PIC S9(3)   VALUE +102      COMP-3.           
006200   03  LAS-AVROP            PIC S9(3)   VALUE +103      COMP-3.           
006300   03  LAS-INLEVERANS       PIC S9(3)   VALUE +104      COMP-3.           
006400   03  LAS-LEV-BESK         PIC S9(3)   VALUE +105      COMP-3.           
006500   03  LAS-LEVERANTORS-REG  PIC S9(3)   VALUE +106      COMP-3.           
006600   03  LAS-BEN-REG          PIC S9(3)   VALUE +107      COMP-3.           
006700   03  LAS-CROSS-INDEX      PIC S9(3)   VALUE +108      COMP-3.           
006800   03  LAS-FRAM-TILL-LEVBSK PIC S9(3)   VALUE +109      COMP-3.           
006900     SKIP3                                                                
007000 01      W-IDLEVNR-X.                                                     
007100   03    W-IDLEVNR       PIC X(5)    VALUE SPACE.                         
007200 01      W-IDBENR-X.                                                      
007300   03    W-IDBENR        PIC S9(1)   COMP-3.                              
007400 01      W-IDARTNR-X.                                                     
007500   03    W-IDARTNR       PIC S9(9)   COMP-3.                              
007600 01      W-WDD901KY-X.                                                    
007700   03    W-IDARTNR-D9    PIC S9(9)   COMP-3.                              
007800   03    W-IDDC-D9       PIC  X(2).                                       
007900 01      W-WDD905KY-X.                                                    
008000     03  W-DAAVROP-X.                                                     
008100         05  W-DAAVROP       PIC  9(6)    VALUE ZERO.                     
008200     03  W-TILEVDAG-X.                                                    
008300         05  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.              
008400 01      W-IDSKYLT-X.                                                     
008500   03    W-IDSKYLT       PIC  X(3).                                       
008600     SKIP3                                                                
008700*                            *** GENERELLA SUBRUTINER                     
008800 01      SUBPROGRAM.                                                      
008900   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
009000   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
009100   03    WDATKONV        PIC X(8)    VALUE 'WDATKONV'.                    
009200     EJECT                                                                
009300*        COPYTEXTER TILL SUBPROGRAM                                       
009400 01      FILLER          PIC X(8)    VALUE 'WDATAREA'.                    
009500*01      -COPY WDATAREAC0.                                                
009600                                                                          
009700     EJECT                                                                
009800*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
009900*                                                                         
010000 01      IMS-WS.                                                          
010100   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
010200     SKIP3                                                                
010300*                            *** STATUSKOD FRÅN IMS                       
010400   03    STATUS-WS       PIC XX.                                          
010500     88  SEGMENT-FINNS               VALUE '  '.                          
010600     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
010700     88  SEGMENT-SLUT                VALUE 'GB'.                          
010800     SKIP3                                                                
010900   03    SSA1            PIC X(40).                                       
011000   03    SSA2            PIC X(40).                                       
011100   03    SSA3            PIC X(40).                                       
011200   03    SSA4            PIC X(40).                                       
011300   03    SSA5            PIC X(40).                                       
011400     SKIP3                                                                
011500   03    GODK-STATUSKODER.                                                
011600     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
011700     EJECT                                                                
011800*01      -COPY W0003                                                      
011900                                                                          
012000     SKIP3                                                                
012100 01  WS-FALT.                                                             
012200     03  WS-TIAVIDAT-SEN-C1      PIC S9(7)     COMP-3.                    
012300     03  WS-IDAVINR-SEN-C1       PIC S9(7)     COMP-3.                    
012400     03  WS-KVAVIS-SEN-C1        PIC S9(7)     COMP-3.                    
012500     03  WS-IDLEVNR-SEN-C1       PIC X(5).                                
012600     SKIP1                                                                
012700     03  WS-TIAVIDAT-SEN-C2      PIC S9(7)     COMP-3.                    
012800     03  WS-IDAVINR-SEN-C2       PIC S9(7)     COMP-3.                    
012900     03  WS-KVAVIS-SEN-C2        PIC S9(7)     COMP-3.                    
013000     03  WS-IDLEVNR-SEN-C2       PIC X(5).                                
013100     EJECT                                                                
013200 01      DLI-IO-AREA-01.                                                  
013300*03  WLARTC01 -COPY WDK601                                                
013400                                                                          
013500     EJECT                                                                
013600 01      DLI-IO-AREA-11.                                                  
013700*03  WLARTC11 -COPY WDK611                                                
013800                                                                          
013900     EJECT                                                                
014000 01      FILLER.                                                          
014100 03      DLI-IO-AREA     PIC X(200)  VALUE SPACE.                         
014200     SKIP3                                                                
014300*03  WLINLB01 -COPY WDD901 -PRE INLB01- -RED DLI-IO-AREA.                 
014400                                                                          
014500     EJECT                                                                
014600*03  WLINLB11 -COPY WDD902 -PRE INLB11- -RED DLI-IO-AREA.                 
014700                                                                          
014800     EJECT                                                                
014900*03  WLINLB23 -COPY WDD905 -PRE INLB23- -RED DLI-IO-AREA.                 
015000                                                                          
015100     EJECT                                                                
015200*03  WLINLB31 -COPY WDD906 -PRE INLB31- -RED DLI-IO-AREA.                 
015300                                                                          
015400     EJECT                                                                
015500*03  WLINLB24 -COPY WDD924 -PRE INLB24- -RED DLI-IO-AREA.                 
015600                                                                          
015700     EJECT                                                                
015800*03  WLLEVA01 -COPY WDF101 -PRE LEVA01- -RED DLI-IO-AREA.                 
015900                                                                          
016000     EJECT                                                                
016100*03  WLBENA11 -COPY WDD311 -PRE BENA11- -RED DLI-IO-AREA.                 
016200                                                                          
016300     EJECT                                                                
016400*03  WDF501   -COPY WDF501 -RED DLI-IO-AREA.                              
016500                                                                          
016600     EJECT                                                                
016700*03  WDF501   -COPY WDF502 -RED DLI-IO-AREA.                              
016800                                                                          
016900     EJECT                                                                
017000 LINKAGE SECTION.                                                         
017100                                                                          
017200 01      LINK-KDCALL     PIC S9(3)        COMP-3.                         
017300                                                                          
017400 01      LINK-AREA       PIC X(200).                                      
017500                                                                          
017600*01  A         -COPY W236L001 -PRE LINK1-  -RED LINK-AREA                 
017700                                                                          
017800     EJECT                                                                
017900*01  B         -COPY W236L002 -PRE LINK2-  -RED LINK-AREA                 
018000                                                                          
018100     EJECT                                                                
018200*01  C         -COPY W236L003 -PRE LINK3-  -RED LINK-AREA                 
018300                                                                          
018400     EJECT                                                                
018500*01  D         -COPY W236L004 -PRE LINK4-  -RED LINK-AREA                 
018600                                                                          
018700     EJECT                                                                
018800*01  E         -COPY W236L005 -PRE LINK5-  -RED LINK-AREA                 
018900                                                                          
019000     EJECT                                                                
019100*01      -COPY W0008     -PRE WDF5-                                       
019200                                                                          
019300      05 FILLER          PIC X.                                           
019400     EJECT                                                                
019500*01      -COPY W0008     -PRE LEVA-                                       
019600                                                                          
019700      05 FILLER          PIC X.                                           
019800     SKIP2                                                                
019900*01      -COPY W0008     -PRE ARTC-                                       
020000                                                                          
020100      05 FILLER          PIC X.                                           
020200     EJECT                                                                
020300*01      -COPY W0008     -PRE INLB-                                       
020400                                                                          
020500      05 FILLER          PIC X.                                           
020600     EJECT                                                                
020700*01      -COPY W0008     -PRE INLB2-                                      
020800                                                                          
020900      05 FILLER          PIC X.                                           
021000     EJECT                                                                
021100*01      -COPY W0008     -PRE BENA-                                       
021200                                                                          
021300      05 FILLER          PIC X.                                           
021400     EJECT                                                                
021500 PROCEDURE DIVISION USING LINK-KDCALL LINK-AREA                           
021600           WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB                            
021700           INLB2-PCB BENA-PCB.                                            
021800                                                                          
021900     ENTRY 'DLITCBL' USING LINK-KDCALL LINK-AREA                          
022000           WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB                            
022100           INLB2-PCB BENA-PCB.                                            
022200                                                                          
022300     SKIP3                                                                
022400     EVALUATE LINK-KDCALL                                                 
022500     WHEN LAS-ART-INFO                                                    
022600       PERFORM A-LAS-ART-INFO                                             
022700     WHEN LAS-LEV-INFO                                                    
022800       PERFORM B-LAS-LEV-INFO                                             
022900     WHEN LAS-AVROP                                                       
023000       PERFORM C-LAS-AVROP                                                
023100     WHEN LAS-INLEVERANS                                                  
023200       PERFORM D-LAS-INLEVERANS                                           
023300     WHEN LAS-LEV-BESK                                                    
023400       PERFORM E-LAS-LEV-BESK                                             
023500     WHEN LAS-LEVERANTORS-REG                                             
023600       PERFORM F-LAS-LEVERANTORS-REG                                      
023700     WHEN LAS-BEN-REG                                                     
023800       PERFORM G-LAS-BEN-REG                                              
023900     WHEN LAS-CROSS-INDEX                                                 
024000       PERFORM H-LAS-CROSS-INDEX                                          
024100     WHEN LAS-FRAM-TILL-LEVBSK                                            
024200       PERFORM I-LAS-FRAM-TILL-LEVBSK                                     
024300     END-EVALUATE                                                         
024400     MOVE ZERO TO RETURN-CODE                                             
024500     GOBACK                                                               
024600     .                                                                    
024700                                                                          
024800     EJECT                                                                
024900 A-LAS-ART-INFO SECTION.                                                  
025000     SKIP1                                                                
025100     MOVE LINK1-IDARTNR TO W-IDARTNR                                      
025200     MOVE SPACE         TO LINK1-IDLEVNR                                  
025300     PERFORM IMS-GET-UNIQE-WLARTC01                                       
025400     MOVE FINNS TO LINK1-KDSVAR                                           
025500     MOVE ART-IDARTNR   TO LINK1-IDARTNR W-IDARTNR                        
025600     MOVE ART-KDERS-UTG TO LINK1-KDERS-UTG                                
025700     MOVE ART-IDLEVNR   TO LINK1-IDLEVNR                                  
025800     MOVE ART-KDPRODSL  TO LINK1-KDPRODSL                                 
025900*                                                                         
026000     IF ART-KDERS-UTG = 0                                                 
026100        MOVE W-IDARTNR  TO W-IDARTNR-D9                                   
026110        MOVE WC-CDC-SE  TO W-IDDC-D9                                      
026200        PERFORM IMS-GET-WLINLB01                                          
026300        IF SEGMENT-SAKNAS                                                 
026400*         MOVE '1000 '  TO LINK1-IDLEVNR                                  
026500          MOVE 9 TO LINK1-KDHF                                            
026600        ELSE                                                              
026700          PERFORM IMS-GET-WLARTC11                                        
026800          MOVE CLAG-IDANSK TO LINK1-IDANSK                                
026900          MOVE CLAG-KDHF   TO LINK1-KDHF                                  
027000*                                                                         
027100          MOVE CLAG-TIAVIDAT-SEN TO WS-TIAVIDAT-SEN-C1                    
027200                                                                          
027300*    -- REDIGERA IDAVINR                                                  
027400          MOVE CLAG-IDFS-SEN     TO WS-IDFS                               
027500          MOVE ZERO              TO WS-IDAVINR                            
027600          MOVE K-IDFS-LNG        TO IX-IDFS                               
027700          MOVE K-IDAVINR-LNG     TO IX-IDAVINR                            
027800          PERFORM UNTIL (IX-IDFS      = ZERO                              
027900                     OR  IX-IDAVINR   = ZERO)                             
028000            IF  WS-IDFS-TKN (IX-IDFS) NUMERIC                             
028100              MOVE WS-IDFS-TKN (IX-IDFS)                                  
028200                                 TO WS-IDAVINR-TKN (IX-IDAVINR)           
028300              SUBTRACT 1       FROM IX-IDAVINR                            
028400            END-IF                                                        
028500            SUBTRACT 1         FROM IX-IDFS                               
028600          END-PERFORM                                                     
028700          MOVE WS-IDAVINR        TO WS-IDAVINR-SEN-C1                     
028800                                                                          
028900          MOVE CLAG-KVAVIS-SEN   TO WS-KVAVIS-SEN-C1                      
029000          MOVE CLAG-IDLEVNR-SEN  TO WS-IDLEVNR-SEN-C1                     
029100            MOVE ZERO TO WS-TIAVIDAT-SEN-C2                               
029200                         WS-IDAVINR-SEN-C2                                
029300                         WS-KVAVIS-SEN-C2                                 
029400                         WS-IDLEVNR-SEN-C2                                
029500          MOVE CLAG-PRARTSTD     TO LINK1-PRARTSTD                        
029600*                                                                         
029700          MOVE CLAG-FLTOPP       TO LINK1-FLTOPP                          
029800          MOVE CLAG-KDGK         TO LINK1-KDGK                            
029900          IF CLAG-KDGK = 1                                                
030000            MOVE WS-TIAVIDAT-SEN-C1 TO LINK1-TIAVIDAT-SEN                 
030100            MOVE WS-IDAVINR-SEN-C1 TO LINK1-IDAVINR-SEN                   
030200            MOVE WS-KVAVIS-SEN-C1 TO LINK1-KVAVIS-SEN                     
030300            MOVE WS-IDLEVNR-SEN-C1 TO LINK1-IDLEVNR-SEN                   
030400          ELSE                                                            
030500            MOVE ZERO TO LINK1-TIAVIDAT-SEN                               
030600            MOVE ZERO TO LINK1-IDAVINR-SEN                                
030700            MOVE ZERO TO LINK1-KVAVIS-SEN                                 
030800            MOVE SPACE   TO LINK1-IDLEVNR-SEN                             
030900          END-IF                                                          
031000          MOVE ZERO TO LINK1-KVAKS-E                                      
031100                       LINK1-KVAKS                                        
031200                       LINK1-KVRESS                                       
031300                       LINK1-KVLS                                         
031400                       LINK1-KVROS                                        
031500                       LINK1-KVSLAGER                                     
031600            ADD CLAG-KVAKS-CDC    TO LINK1-KVAKS                          
031700            ADD CLAG-KVAKS-PAV    TO LINK1-KVAKS                          
031800            ADD CLAG-KVAKS-T      TO LINK1-KVAKS                          
031900            ADD CLAG-KVRESS       TO LINK1-KVRESS                         
032000            ADD CLAG-KVLS         TO LINK1-KVLS                           
032100            ADD CLAG-KVROS        TO LINK1-KVROS                          
032200            ADD CLAG-KVSLAGER     TO LINK1-KVSLAGER                       
032300        END-IF                                                            
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 B-LAS-LEV-INFO SECTION.                                                  
032800     SKIP1                                                                
032900     MOVE SAKNAS TO LINK1-KDSVAR                                          
033000     PERFORM IMS-GET-WLINLB11                                             
033100     IF SEGMENT-FINNS                                                     
033200       MOVE FINNS TO LINK1-KDSVAR                                         
033300       MOVE INLB11-IDLEVNR TO LINK1-IDLEVNR W-IDLEVNR                     
033400       MOVE INLB11-KVBR  TO LINK1-KVBR                                    
033500     END-IF                                                               
033600     .                                                                    
033700     SKIP3                                                                
033800 C-LAS-AVROP SECTION.                                                     
033900     SKIP1                                                                
034000     MOVE SAKNAS TO LINK2-KDSVAR                                          
034100     PERFORM IMS-GET-WLINLB23                                             
034200     IF SEGMENT-FINNS                                                     
034300       MOVE FINNS TO LINK2-KDSVAR                                         
034400       MOVE INLB23-KDAVROP TO LINK2-KDAVROP                               
034500       MOVE INLB23-DAAVROP-AVS TO WS-DAAVROP-AVS    W-DAAVROP             
034600       MOVE INLB23-TILEVDAG    TO W-TILEVDAG                              
034700       MOVE WS-DAAVROP-AAVV    TO LINK2-TIAVROP-AVS                       
034800       MOVE INLB23-KVAVROP     TO LINK2-KVAVROP                           
034900       MOVE INLB23-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
035000       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
035100       CALL WDATKONV USING          DAT-KDDATFORM                         
035200                                    DAT-I-TIDATUM                         
035300                                    DAT-O-TIDATUM                         
035400                                    DAT-KDSVAR                            
035500       IF DAT-KDSVAR-FEL                                                  
035600         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
035700         CALL FELLOG                                                      
035800       ELSE                                                               
035900         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
036000         MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL                     
036100         MOVE DAT-TID            TO LINK2-TID                             
036200       END-IF                                                             
036300     ELSE                                                                 
036400       MOVE ZERO TO LINK2-KDAVROP                                         
036500                    LINK2-TIAVROP-AVS                                     
036600                    LINK2-TIAVROP-INL                                     
036700                    LINK2-KVAVROP                                         
036800                    LINK2-TID                                             
036900     END-IF                                                               
037000     .                                                                    
037100     SKIP3                                                                
037200 D-LAS-INLEVERANS SECTION.                                                
037300     SKIP1                                                                
037400     MOVE SAKNAS TO LINK2-KDSVAR                                          
037500     PERFORM IMS-GET-WLINLB31                                             
037600     IF SEGMENT-FINNS                                                     
037700       MOVE FINNS TO LINK2-KDSVAR                                         
037800       MOVE INLB31-IDLOPNRM-PL TO LINK2-IDLOPNRM-PL                       
037900       MOVE INLB31-KVAVROP-AVB TO LINK2-KVAVROP-AVB                       
038000     END-IF                                                               
038100     .                                                                    
038200 E-LAS-LEV-BESK SECTION.                                                  
038300     SKIP1                                                                
038400     MOVE SAKNAS TO LINK3-KDSVAR                                          
038500     PERFORM IMS-GET-WLINLB24                                             
038600     IF SEGMENT-FINNS                                                     
038700       MOVE FINNS TO LINK3-KDSVAR                                         
038800       MOVE 'AAMMDD'            TO DAT-KDDATFORM                          
038900       MOVE INLB24-LEV-DALEVBSK-AVS TO WS-DALEVBSK-AVS                    
039000       MOVE WS-DALEVBSK-AAMMDD      TO DAT-I-TIDATUM                      
039100       CALL WDATKONV USING DAT-KDDATFORM                                  
039200       DAT-I-TIDATUM                                                      
039300       DAT-O-TIDATUM                                                      
039400       DAT-KDSVAR                                                         
039500       IF DAT-KDSVAR-OK                                                   
039600         MOVE DAT-TIAAVVD TO WS-TIAAVVD                                   
039700         MOVE WS-TIAAVV   TO LINK3-TILEVBSK-AVS                           
039800       END-IF                                                             
039900                                                                          
040000         MOVE INLB24-LEV-KVAVIS-BSKKVAR    TO                             
040100         LINK3-KVAVIS-BSKKVAR                                             
040200     END-IF                                                               
040300     .                                                                    
040400     SKIP3                                                                
040500 F-LAS-LEVERANTORS-REG SECTION.                                           
040600     SKIP1                                                                
040700     MOVE LINK4-IDLEVNR TO W-IDLEVNR                                      
040800     PERFORM IMS-GET-WLLEVA01                                             
040900     IF SEGMENT-FINNS                                                     
041000       MOVE LEVA01-LEV-KDSPRAK TO LINK4-KDSPRAK                           
041100     ELSE                                                                 
041200       MOVE 0 TO LINK4-KDSPRAK                                            
041300     END-IF                                                               
041400     .                                                                    
041500     SKIP3                                                                
041600 G-LAS-BEN-REG SECTION.                                                   
041700     SKIP1                                                                
041800     MOVE LINK4-IDARTNR  TO  W-IDARTNR                                    
041900     MOVE LINK4-IDSKYLT  TO  W-IDSKYLT                                    
042000     PERFORM IMS-GET-BENA11-BSEQ                                          
042100     MOVE BENA11-TEXT-BEART  TO LINK4-BEART                               
042200     .                                                                    
042300                                                                          
042400     EJECT                                                                
042500 H-LAS-CROSS-INDEX SECTION.                                               
042600     SKIP1                                                                
042700     MOVE LINK4-IDARTNR TO W-IDARTNR                                      
042800     MOVE LINK4-IDLEVNR TO W-IDLEVNR                                      
042900     MOVE +1            TO W-IDBENR                                       
043000     PERFORM IMS-GET-CROSS-BELEV                                          
043100     IF SEGMENT-FINNS                                                     
043200       MOVE XLEV-BELEVART TO LINK4-BELEV                                  
043300     ELSE                                                                 
043400       MOVE SPACE TO LINK4-BELEV                                          
043500     END-IF                                                               
043600     .                                                                    
043700     SKIP3                                                                
043800 I-LAS-FRAM-TILL-LEVBSK SECTION.                                          
043900*                                                                         
044000     MOVE LINK5-IDARTNR  TO  W-IDARTNR                                    
044100     MOVE LINK5-IDLEVNR  TO  W-IDLEVNR                                    
044200     PERFORM IMS-GET-UNIQE-WLINLB11                                       
044300     .                                                                    
044400                                                                          
044500     EJECT                                                                
044600* IMS SECTIONER                                                           
044700     SKIP3                                                                
044800 IMS-GET-UNIQE-WLARTC01 SECTION.                                          
044900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045000     DELIMITED BY SIZE INTO SSA1                                          
045100     MOVE '  ' TO GODK-STATUSKODER                                        
045200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
045300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     SKIP3                                                                
045700 IMS-GET-WLARTC11 SECTION.                                                
045800     MOVE 'WLARTC11*F(KDSEGKEY =1)' TO SSA1                               
045900     MOVE '  ' TO GODK-STATUSKODER                                        
046000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
046100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GET-WLINLB01 SECTION.                                                
046600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
046700     DELIMITED BY SIZE INTO SSA1                                          
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
047000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     SKIP3                                                                
047400 IMS-GET-WLINLB11 SECTION.                                                
047500     MOVE 'WLINLB11 ' TO SSA1                                             
047600     MOVE '  GE' TO GODK-STATUSKODER                                      
047700     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
047800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
047900     PERFORM IMS-STATUSKONTROLL                                           
048000     .                                                                    
048100     SKIP3                                                                
048200 IMS-GET-UNIQE-WLINLB11 SECTION.                                          
048300     STRING 'WLINLB01(IDARTNR  =' W-IDARTNR-X ')'                         
048400     DELIMITED BY SIZE INTO SSA1                                          
048500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
048600     DELIMITED BY SIZE INTO SSA2                                          
048700     MOVE '  ' TO GODK-STATUSKODER                                        
048800     CALL CBLTDLI USING GU INLB2-PCB DLI-IO-AREA SSA1 SSA2                
048900     MOVE INLB2-STATUS-CODE TO STATUS-WS                                  
049000     PERFORM IMS-STATUSKONTROLL                                           
049100     .                                                                    
049200     EJECT                                                                
049300 IMS-GET-WLINLB23 SECTION.                                                
049400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
049500     DELIMITED BY SIZE INTO SSA1                                          
049600     MOVE 'WLINLB23 ' TO SSA2                                             
049700     MOVE '  GE' TO GODK-STATUSKODER                                      
049800     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
049900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
050100     .                                                                    
050200     EJECT                                                                
050300 IMS-GET-WLINLB31 SECTION.                                                
050400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
050500     DELIMITED BY SIZE INTO SSA1                                          
050600     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X ')'                        
050700     DELIMITED BY SIZE INTO SSA2                                          
050800     MOVE 'WLINLB31 ' TO SSA3                                             
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA                          
051100                        SSA1 SSA2 SSA3                                    
051200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
051300     PERFORM IMS-STATUSKONTROLL                                           
051400     .                                                                    
051500     SKIP3                                                                
051600 IMS-GET-WLINLB24 SECTION.                                                
051700     MOVE 'WLINLB24 ' TO SSA1                                             
051800     MOVE '  GE' TO GODK-STATUSKODER                                      
051900     CALL CBLTDLI USING GNP INLB2-PCB DLI-IO-AREA SSA1                    
052000     MOVE INLB2-STATUS-CODE TO STATUS-WS                                  
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     EJECT                                                                
052400 IMS-GET-BENA11-BSEQ SECTION.                                             
052500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
052600     DELIMITED BY SIZE INTO SSA1                                          
052700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
052800     DELIMITED BY SIZE INTO SSA2                                          
052900     MOVE '  ' TO GODK-STATUSKODER                                        
053000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
053100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     .                                                                    
053400     SKIP3                                                                
053500 IMS-GET-CROSS-BELEV SECTION.                                             
053600     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
053700     DELIMITED BY SIZE INTO SSA1                                          
053800     STRING 'WDF502  (WDF5KEY  =' W-IDLEVNR-X W-IDBENR-X ')'              
053900     DELIMITED BY SIZE INTO SSA2                                          
054000     MOVE '  GE' TO GODK-STATUSKODER                                      
054100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1 SSA2                 
054200     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
054300     PERFORM IMS-STATUSKONTROLL                                           
054400     .                                                                    
054500     SKIP3                                                                
054600 IMS-GET-WLLEVA01 SECTION.                                                
054700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
054800     DELIMITED BY SIZE INTO SSA1                                          
054900     MOVE '  GE' TO GODK-STATUSKODER                                      
055000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
055100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
055200     PERFORM IMS-STATUSKONTROLL                                           
055300     .                                                                    
055400     SKIP3                                                                
055500 IMS-STATUSKONTROLL SECTION.                                              
055600     SET STATUS-IX TO 1                                                   
055700     SEARCH GODK-STATUS AT END CALL FELLOG                                
055800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
055900     CONTINUE                                                             
056000     END-SEARCH                                                           
056100     .                                                                    
056200     EJECT                                                                
