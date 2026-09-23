000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4760500.                                                
000003 AUTHOR.         STEFAN KIHLBERG.                                         
000004 DATE-WRITTEN.   02/11/22.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNCTION:                                                            
000008*        HÄMTAR PRODNR SOM SKALL AUTOMATFAKTURERAS FRÅN HÄNDELSE          
000009*        4627.LÄGGER UTIFRÅN UPPGIFTER FRÅN E6 UPP SHIPMENTS PÅ           
000010*        WDE2 (WDE1), STARTAR ADD-IT                                      
000011*                                                                         
000012*        THE PROGRAM UPDATES   WDG7                                       
000013*        THE PROGRAM UPDATES   WDE6                                       
000014*        THE PROGRAM UPDATES   WDE1                                       
000015*        THE PROGRAM UPDATES   WDE2                                       
000016*                                                                         
000017                                                                          
000018     SKIP3                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000023 FILE-CONTROL.                                                            
000024     SELECT  W476BMP                   ASSIGN TO W47605D1.                
000025     EJECT                                                                
000026 DATA DIVISION.                                                           
000027     SKIP3                                                                
000028 FILE SECTION.                                                            
000029 FD  W476BMP                                                              
000030     LABEL RECORD STANDARD                                                
000031     RECORDING F                                                          
000032     BLOCK CONTAINS 0.                                                    
000033                                                                          
000034 01  FILLER                  PIC X(80).                                   
000035                                                                          
000036     EJECT                                                                
000037 WORKING-STORAGE SECTION.                                                 
000038                                                                          
000039 77  IDPGM                       PIC X(8)    VALUE 'W4760500'.            
000040 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
000041 77  YES                         PIC X       VALUE 'J'.                   
000042 77  NOO                         PIC X       VALUE 'N'.                   
000043 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
000044 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
000045 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
000046                                                                          
000047 01  CHKP-VAR.                                                            
000048     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000049     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000050     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000051     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000052     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000053*    03 CHKP-MAX                 PIC S9(3)   VALUE +400 COMP-3.           
000054     03 CHKP-MAX                 PIC S9(3)   VALUE +12  COMP-3.           
000055     SKIP2                                                                
000056 01  ERRTEXT.                                                             
000057     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
000058     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
000059     EJECT                                                                
000060     03  NATT-BMP-SW             PIC X       VALUE 'N'.                   
000061     88  NATT-BMP                            VALUE 'J'.                   
000062                                                                          
000063 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
000064 01  FILLER REDEFINES TODAYS-DATE.                                        
000065     03  TODAYS-DATE-YEAR        PIC 9(2).                                
000066     03  TODAYS-DATE-MONTH       PIC 9(2).                                
000067     03  TODAYS-DATE-DAY         PIC 9(2).                                
000068     EJECT                                                                
000069 01  FILLER                      PIC X(16)   VALUE 'WORK-FIELDS'.         
000070 01  WORK-FIELDS.                                                         
000071     03 WS-KVKOLLI-FL            PIC S9(5)      COMP-3 VALUE ZERO.        
000072     03 WS-SUORDV-FL             PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
000073     03 WS-SUORDV-LOC-FL         PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
000074     03 WS-SUORDV-LOCPREL-FL     PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
000075     03 WS-VKORDBTO-FL           PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
000076     03 WS-VLORDBTO-FL           PIC S9(4)V9(3) COMP-3 VALUE ZERO.        
000077     03 WS-IDDISTR               PIC S9(5)   VALUE ZERO COMP-3.           
000078     03 WS-IDKUNDNR              PIC S9(7)   VALUE ZERO COMP-3.           
000079     03 WS-IDDC                  PIC X(2)    VALUE SPACE.                 
000080     03 WS-KDFAKTYP              PIC X(1)    VALUE SPACE.                 
000081                                                                          
000082 01  TEST-IDDISTR                PIC  9(5) COMP-3 VALUE ZERO.             
000083*01  FILLER  -COPY WWDIST03   -RED TEST-IDDISTR.                          
000084*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
000085*01  FILLER  -COPY WWDIST38   -RED TEST-IDDISTR.                          
000086     EJECT                                                                
000087*                                                                         
000088       EJECT                                                              
000089 01  GENERAL-SUBPROGRAMS.                                                 
000090*                                                                         
000091     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000092     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000093     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000094     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
000095     EJECT                                                                
000096 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND '.           
000097*    --- PARAMETRAR TILL SUBPROGRAM WZ01SEND                              
000098*01 -COPY WZ01SEND                                                        
000099*                                                                         
000100 01  FILLER                      PIC X(16)   VALUE 'W476SHNO '.           
000101*    --- PARAMETRAR TILL SUBPROGRAM W476SHNO                              
000102*01  -COPY W476SHNO                                                       
000103*                                                                         
000104                                                                          
000105     EJECT                                                                
000106 01 W-INPOST.                                                             
000107     03  W-IDBMPTYP              PIC X(4).                                
000108     03  FILLER                  PIC X(76).                               
000109                                                                          
000110 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000111     SKIP3                                                                
000112 01  KEYS-TILL-DLI.                                                       
000113     03  W-4726-X.                                                        
000114        05  FILLER               PIC X(4)    VALUE '4726'.                
000115        05  4726-FLBATCH         PIC X(1).                                
000116        05  FILLER               PIC X(25)   VALUE LOW-VALUE.             
000117     03  W-WDGXKEY-X.                                                     
000118         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000119         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000120         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000121         05  W-KDFAKTYP          PIC X(1)    VALUE SPACE.                 
000122     03  W-IDPRODNR-X.                                                    
000123         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
000124     03  W-IDKOLLI-X.                                                     
000125         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
000126     03  W-KDKOLSTA-X.                                                    
000127         05  W-KDKOLSTA          PIC S9(1)   VALUE ZERO COMP-3.           
000128     03  W-IDSHIPM-X.                                                     
000129         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
000130                                                                          
000131     03  W-WDE111KY-X.                                                    
000132         05  W-WDE111-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.           
000133         05  W-WDE111-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.           
000134                                                                          
000135     03  W-WDE211KY-X.                                                    
000136         05  W-WDE211-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.           
000137         05  W-WDE211-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.           
000138                                                                          
000139     03  W-IDDC-B6-X.                                                     
000140         05 W-IDDC-B6                  PIC X(2).                          
000141                                                                          
000142     SKIP2                                                                
000143*01  -COPY W40636I1   -PRE MOD4636-                                       
000144     EJECT                                                                
000145 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
000146 01  KOM-IO-AREA.                                                         
000147*03  -COPY WMSGKOM                                                        
000148     EJECT                                                                
000149*                                                                         
000150 01  FILLER                      PIC X(16)   VALUE 'IMS-AREA'.            
000151*    --- STATUS-KOD FRÅN IMS                                              
000152 01  STATUS-WS                   PIC XX.                                  
000153     88  SEGMENT-FOUND                       VALUE '  '.                  
000154     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000155     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000156     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
000157     88  IMS-NOT-OK                          VALUE 'XD'.                  
000158     SKIP2                                                                
000159 01  GOOD-STATUSCODES.                                                    
000160     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000161     SKIP3                                                                
000162 01  SSA1                        PIC X(128).                              
000163 01  SSA2                        PIC X(128).                              
000164 01  SSA3                        PIC X(128).                              
000165     EJECT                                                                
000166*    --- IMS FUNCTION CODES                                               
000167*01  -COPY W0003                                                          
000168     EJECT                                                                
000169*    ---  DLI INPUT-OUTPUT AREA                                           
000170                                                                          
000171 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX01'.           
000172 01  DLI-IO-WDGX01.                                                       
000173*    03  -COPY WDGX01                                                     
000174     EJECT                                                                
000175 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX4726'.         
000176 01  DLI-IO-WDGX4726.                                                     
000177*    03  -COPY WDGX4726                                                   
000178     EJECT                                                                
000179 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX4727'.         
000180 01  DLI-IO-WDGX4727.                                                     
000181*    03  -COPY WDGX4727                                                   
000182 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE601'.           
000183 01  DLI-IO-WDE601.                                                       
000184*    03  -COPY WDE601                                                     
000185     EJECT                                                                
000186 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE611'.           
000187 01  DLI-IO-WDE611.                                                       
000188*    03  -COPY WDE611                                                     
000189 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE101'.           
000190 01  DLI-IO-WDE101.                                                       
000191*    03  -COPY WDE101                                                     
000192 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE111'.           
000193 01  DLI-IO-WDE111.                                                       
000194*    03  -COPY WDE111                                                     
000195 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE121'.           
000196 01  DLI-IO-WDE121.                                                       
000197*    03  -COPY WDE121                                                     
000198 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE201'.           
000199 01  DLI-IO-WDE201.                                                       
000200*    03  -COPY WDE201                                                     
000201     EJECT                                                                
000202 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE211'.           
000203 01  DLI-IO-WDE211.                                                       
000204*    03  -COPY WDE211                                                     
000205     EJECT                                                                
000206 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE221'.           
000207 01  DLI-IO-WDE221.                                                       
000208*    03  -COPY WDE221                                                     
000209                                                                          
000210 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000211 01   DLI-IO-AREA-B601.                                                   
000212*     03  -COPY WDB601                                                    
000213                                                                          
000214     EJECT                                                                
000215 LINKAGE SECTION.                                                         
000216                                                                          
000217*01  -COPY W0009   -PRE MSG-                                              
000218                                                                          
000219*01  -COPY W0008  -PRE AD36-                                              
000220     05  FILLER                  PIC X.                                   
000221                                                                          
000222*01  -COPY W0008  -PRE 4726-                                              
000223     05  FILLER                  PIC X.                                   
000224                                                                          
000225*01  -COPY W0008  -PRE WDE6-                                              
000226     05  FILLER                  PIC X.                                   
000227                                                                          
000228*01  -COPY W0008  -PRE WDE1-                                              
000229     05  FILLER                  PIC X.                                   
000230                                                                          
000231*01  -COPY W0008  -PRE WDE2-                                              
000232     05  FILLER                  PIC X.                                   
000233                                                                          
000234*01  -COPY W0008  -PRE WDB6-                                              
000235     05  FILLER                  PIC X.                                   
000236                                                                          
000237*01  -COPY W0008  -PRE 4517-                                              
000238     05  FILLER                  PIC X.                                   
000239     EJECT                                                                
000240 PROCEDURE DIVISION  USING MSG-PCB AD36-PCB                               
000241                           4726-PCB WDE6-PCB                              
000242                           WDE1-PCB WDE2-PCB                              
000243                           WDB6-PCB                                       
000244                           4517-PCB.                                      
000245 MAIN SECTION.                                                            
000246     ENTRY 'DLITCBL' USING MSG-PCB AD36-PCB                               
000247                           4726-PCB WDE6-PCB                              
000248                           WDE1-PCB WDE2-PCB                              
000249                           WDB6-PCB                                       
000250                           4517-PCB.                                      
000251                                                                          
000252     PERFORM A-INIT                                                       
000253     PERFORM IMS-AUTFAKT-GET-ROTSEG                                       
000254     IF SEGMENT-FOUND                                                     
000255       PERFORM IMS-AUTFAKT-GNP-DISTSEG-OKVAL                              
000256       PERFORM UNTIL SEGMENT-MISSING                                      
000257         IF CHKP-ANT > CHKP-MAX                                           
000258                                                                          
000259           PERFORM X-TAKE-CHECKPOINT                                      
000260           PERFORM IMS-AUTFAKT-GET-ROTSEG                                 
000261           PERFORM S03-FLYTTA-NYCKLAR                                     
000262           PERFORM IMS-AUTFAKT-GNP-DISTSEG                                
000263         END-IF                                                           
000264         MOVE AUTFAKT-IDDISTR  TO TEST-IDDISTR                            
000265         PERFORM S03-FLYTTA-NYCKLAR                                       
000266         PERFORM IMS-AUTFAKT-GNP-ORDERSEG                                 
000267         PERFORM UNTIL SEGMENT-MISSING                                    
000268           PERFORM B-BEHANDLA-PRODNUMMER                                  
000269           PERFORM IMS-AUTFAKT-GNP-ORDERSEG                               
000270         END-PERFORM                                                      
000271         PERFORM IMS-AUTFAKT-GHNP-DISTSEG                                 
000272         PERFORM IMS-AUTFAKT-DLET-DISTSEG                                 
000273         ADD +1                TO CHKP-ANT                                
000274         PERFORM IMS-AUTFAKT-GNP-DISTSEG-OKVAL                            
000275       END-PERFORM                                                        
000276     END-IF                                                               
000277                                                                          
000278     PERFORM Z-FINIT                                                      
000279                                                                          
000280     MOVE ZERO TO RETURN-CODE                                             
000281     GOBACK                                                               
000282     .                                                                    
000283     EJECT                                                                
000284 A-INIT SECTION.                                                          
000285     SKIP2                                                                
000286                                                                          
000287     OPEN INPUT W476BMP                                                   
000288                                                                          
000289     READ W476BMP             INTO W-INPOST                               
000290     IF W-IDBMPTYP             = 'NATT'                                   
000291       MOVE YES                  TO 4726-FLBATCH                          
000292     ELSE                                                                 
000293       MOVE NOO                  TO 4726-FLBATCH                          
000294     END-IF                                                               
000295                                                                          
000296                                                                          
000297     PERFORM IMS-RESTART                                                  
000298                                                                          
000299                                                                          
000300                                                                          
000301     .                                                                    
000302     EJECT                                                                
000303                                                                          
000304 B-BEHANDLA-PRODNUMMER SECTION.                                           
000305                                                                          
000306     MOVE AUTFAKT-IDPRODNR TO W-IDPRODNR                                  
000307     PERFORM IMS-GET-WDE601                                               
000308     IF SEGMENT-FOUND                                                     
000309       PERFORM BA-SKAPA-HUVUD-OCH-KUND                                    
000310       PERFORM IMS-GHNP-WDE611                                            
000311       PERFORM UNTIL SEGMENT-MISSING                                      
000312         IF KOLLI-KDKOLSTA = +1                                           
000313            PERFORM BB-SAMLA-KOLLIDATA                                    
000314            MOVE +6 TO KOLLI-KDKOLSTA                                     
000315            PERFORM IMS-REPL-WDE611                                       
000316            ADD +1          TO CHKP-ANT                                   
000317            PERFORM BC-SKAPA-KOLLIN                                       
000318         END-IF                                                           
000319         PERFORM IMS-GHNP-WDE611                                          
000320       END-PERFORM                                                        
000321       PERFORM S23-OPEN-WZ01                                              
000322       PERFORM S24-SEND-WZ01                                              
000323       PERFORM S25-CLOSE-WZ01                                             
000324       MOVE AUTFAKT-IDPRODNR TO W-IDPRODNR                                
000325       PERFORM IMS-GHU-WDE601                                             
000326       PERFORM BD-UPPDATERA-E601                                          
000327       PERFORM IMS-REPL-WDE601                                            
000328       ADD +1          TO CHKP-ANT                                        
000329     END-IF                                                               
000330     .                                                                    
000331     EJECT                                                                
000332                                                                          
000333 BA-SKAPA-HUVUD-OCH-KUND SECTION.                                         
000334                                                                          
000335     PERFORM S01-CREATE-IDSHIPM                                           
000336     PERFORM S02-DC-LAND                                                  
000337     PERFORM S101-CREATE-SHIP-SEG                                         
000338     PERFORM IMS-ISRT-WDE101                                              
000339     PERFORM S111-CREATE-SHIP-GMT-SEG                                     
000340     PERFORM IMS-ISRT-WDE111                                              
000341     PERFORM S201-CREATE-BILL-SEG                                         
000342     PERFORM IMS-ISRT-WDE201                                              
000343     PERFORM S211-CREATE-BILL-GMT-SEG                                     
000344     PERFORM IMS-ISRT-WDE211                                              
000345     ADD +4          TO CHKP-ANT                                          
000346     .                                                                    
000347     EJECT                                                                
000348                                                                          
000349 BB-SAMLA-KOLLIDATA SECTION.                                              
000350                                                                          
000351     COMPUTE WS-KVKOLLI-FL =                                              
000352             WS-KVKOLLI-FL + 1                                            
000353     COMPUTE WS-SUORDV-FL =                                               
000354             WS-SUORDV-FL + KOLLI-SUORDV-KOLLI                            
000355     COMPUTE WS-SUORDV-LOC-FL =                                           
000356             WS-SUORDV-LOC-FL + KOLLI-SUORDV-LOC                          
000357     COMPUTE WS-SUORDV-LOCPREL-FL =                                       
000358             WS-SUORDV-LOCPREL-FL + KOLLI-SUORDV-LOCPREL                  
000359     COMPUTE WS-VKORDBTO-FL =                                             
000360             WS-VKORDBTO-FL + KOLLI-VKORDBTO-KOLLI                        
000361     COMPUTE WS-VLORDBTO-FL =                                             
000362             WS-VLORDBTO-FL + KOLLI-VLORDBTO-KOLLI                        
000363     .                                                                    
000364     EJECT                                                                
000365                                                                          
000366 BC-SKAPA-KOLLIN SECTION.                                                 
000367                                                                          
000368     PERFORM S121-CREATE-SHIP-KOL-SEG                                     
000369     PERFORM IMS-ISRT-WDE121                                              
000370     PERFORM S221-CREATE-BILL-KOL-SEG                                     
000371     PERFORM IMS-ISRT-WDE221                                              
000372     ADD +2          TO CHKP-ANT                                          
000373     .                                                                    
000374     EJECT                                                                
000375                                                                          
000376 BD-UPPDATERA-E601 SECTION.                                               
000377                                                                          
000378     COMPUTE VORD-KVKOLLI-FL =                                            
000379             VORD-KVKOLLI-FL + WS-KVKOLLI-FL                              
000380     COMPUTE VORD-SUORDV-FL =                                             
000381             VORD-SUORDV-FL + WS-SUORDV-FL                                
000382     COMPUTE VORD-SUORDV-FL-LOC =                                         
000383             VORD-SUORDV-FL-LOC + WS-SUORDV-LOC-FL                        
000384     COMPUTE VORD-SUORDV-FL-LOCPREL =                                     
000385             VORD-SUORDV-FL-LOCPREL + WS-SUORDV-LOCPREL-FL                
000386     COMPUTE VORD-VKORDBTO-FL =                                           
000387             VORD-VKORDBTO-FL + WS-VKORDBTO-FL                            
000388     COMPUTE VORD-VLORDBTO-FL =                                           
000389             VORD-VLORDBTO-FL + WS-VLORDBTO-FL                            
000390                                                                          
000391     MOVE ZERO                   TO WS-KVKOLLI-FL                         
000392                                    WS-SUORDV-FL                          
000393                                    WS-SUORDV-LOC-FL                      
000394                                    WS-SUORDV-LOCPREL-FL                  
000395                                    WS-VKORDBTO-FL                        
000396                                    WS-VLORDBTO-FL                        
000397     .                                                                    
000398     EJECT                                                                
000399 S01-CREATE-IDSHIPM SECTION.                                              
000400                                                                          
000401     CALL W476SHNO USING SHNO-W476SHNO 4517-PCB                           
000402                                                                          
000403     ADD +1                       TO CHKP-ANT                             
000404     .                                                                    
000405     EJECT                                                                
000406                                                                          
000407 S02-DC-LAND  SECTION.                                                    
000408                                                                          
000409     IF VORD-IDDC NOT = W-IDDC-B6                                         
000410        MOVE VORD-IDDC TO W-IDDC-B6                                       
000411        PERFORM IMS-GU-WDB601                                             
000412     END-IF                                                               
000413     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
000414     .                                                                    
000415     EJECT                                                                
000416                                                                          
000417 S03-FLYTTA-NYCKLAR SECTION.                                              
000418                                                                          
000419     MOVE AUTFAKT-IDDISTR    TO W-IDDISTR                                 
000420                                WS-IDDISTR                                
000421     MOVE AUTFAKT-IDKUNDNR   TO W-IDKUNDNR                                
000422                                WS-IDKUNDNR                               
000423     MOVE AUTFAKT-IDDC       TO W-IDDC                                    
000424                                WS-IDDC                                   
000425     MOVE AUTFAKT-KDFAKTYP   TO W-KDFAKTYP                                
000426                                WS-KDFAKTYP                               
000427     .                                                                    
000428     EJECT                                                                
000429                                                                          
000430 S23-OPEN-WZ01 SECTION.                                                   
000431                                                                          
000432     MOVE 'OPEN'                     TO SEND-KDFUNC                       
000433     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
000434     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
000435                                                                          
000436     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
000437                                SEND-OPEN-AREA                            
000438     IF SEND-KDRC > 0                                                     
000439       MOVE SEND-KDRC           TO KDRC-DISP                              
000440       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
000441            DELIMITED BY SIZE INTO ERROR-TEXT                             
000442       DISPLAY ERROR-TEXT                                                 
000443       CALL FELLOG                                                        
000444     ELSE                                                                 
000445       MOVE SEND-IDCOM               TO WS-IDCOM                          
000446     END-IF                                                               
000447     .                                                                    
000448     EJECT                                                                
000449 S24-SEND-WZ01 SECTION.                                                   
000450                                                                          
000451     MOVE SHNO-IDSHIPM       TO MOD4636-MID-IDSHIPM                       
000452     MOVE 'N'                TO MOD4636-MID-KDTRPINF                      
000453     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
000454                                MOD4636-MID-IDKUNDNR                      
000455                                MOD4636-MID-IDPRODNR                      
000456                                MOD4636-MID-IDKOLLI                       
000457                                MOD4636-MID-SUORDV-DIST                   
000458                                MOD4636-MID-SUORDV-DIST-LOC               
000459                                MOD4636-MID-SUORDV-DIST-PREL              
000460                                MOD4636-MID-SUORDV-NOLL                   
000461                                MOD4636-MID-SUORDV-NOLL-LOC               
000462                                MOD4636-MID-SUORDV-NOLL-PREL              
000463                                MOD4636-MID-KDORDKL                       
000464     MOVE 'PUT'                      TO SEND-KDFUNC                       
000465     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
000466                                                                          
000467     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
000468                                SEND-KVDLEN                               
000469                                MOD4636-MID-W40636I1                      
000470     IF SEND-KDRC > 0                                                     
000471       MOVE SEND-KDRC           TO KDRC-DISP                              
000472       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
000473            DELIMITED BY SIZE INTO ERROR-TEXT                             
000474       DISPLAY ERROR-TEXT                                                 
000475       CALL FELLOG                                                        
000476     END-IF                                                               
000477     .                                                                    
000478     EJECT                                                                
000479 S25-CLOSE-WZ01  SECTION.                                                 
000480                                                                          
000481     MOVE 'CLOSE'               TO SEND-KDFUNC                            
000482                                                                          
000483     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
000484     IF SEND-KDRC > 0                                                     
000485       MOVE SEND-KDRC           TO KDRC-DISP                              
000486       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
000487            DELIMITED BY SIZE INTO ERROR-TEXT                             
000488       DISPLAY ERROR-TEXT                                                 
000489       CALL FELLOG                                                        
000490     END-IF                                                               
000491     .                                                                    
000492     EJECT                                                                
000493 S101-CREATE-SHIP-SEG SECTION.                                            
000494                                                                          
000495     MOVE SHNO-IDSHIPM            TO SHIP-IDSHIPM                         
000496                                      W-IDSHIPM                           
000497     MOVE +999                    TO SHIP-IDTRPTNR                        
000498     MOVE VORD-IDDISTR            TO TEST-IDDISTR                         
000499     IF DIST03-SVERIGE                                                    
000500        IF DCS-CDC                                                        
000501        OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                              
000502        OR DIST18-SKROT                                                   
000503*    IF 4726-FLBATCH = YES                                                
000504         MOVE 'AUTF SVERIGE'      TO SHIP-IDLBBET                         
000505        ELSE                                                              
000506         MOVE 'AUTFAKT'           TO SHIP-IDLBBET                         
000507         IF VORD-KDFRAKT = 88                                             
000508          MOVE 'SOFTAUTF'         TO SHIP-IDLBBET                         
000509         END-IF                                                           
000510        END-IF                                                            
000511     ELSE                                                                 
000512        MOVE 'AUTFAKT'            TO SHIP-IDLBBET                         
000513        IF VORD-KDFRAKT = 88                                              
000514          MOVE 'SOFTAUTF'         TO SHIP-IDLBBET                         
000515        END-IF                                                            
000516     END-IF                                                               
000517     MOVE VORD-IDDC               TO SHIP-IDDC                            
000518     MOVE W-IDLANDX2              TO SHIP-IDLANDX3-SEND                   
000519     MOVE 'N'                     TO SHIP-FLSKRIV-NU                      
000520     MOVE 'N'                     TO SHIP-KDKLAR                          
000521     MOVE ZERO                    TO SHIP-KVANTEX                         
000522                                     SHIP-SUNTO-TOT                       
000523                                     SHIP-PRKURS-BET                      
000524     EVALUATE TRUE                                                        
000525     WHEN VORD-KDFAKTYP = 'R' OR 'G'                                      
000526       MOVE 'INV'                     TO SHIP-KDFINDOC                    
000527     WHEN VORD-KDFAKTYP = 'K' OR 'N'                                      
000528       MOVE 'INT'                     TO SHIP-KDFINDOC                    
000529     END-EVALUATE                                                         
000530     MOVE FUNCTION CURRENT-DATE(3:6)                                      
000531                                  TO SHIP-TISKEPPN                        
000532     MOVE FUNCTION CURRENT-DATE(9:6)                                      
000533                                  TO SHIP-TISKPTID                        
000534     MOVE VORD-IDDC-EXP           TO SHIP-IDDC-EXP                        
000535     MOVE '0'                     TO SHIP-KDFAKSTA-EXP                    
000536     MOVE SPACE                   TO SHIP-BELEVVIL                        
000537                                     SHIP-IDSYSTEM                        
000538                                     SHIP-KDVALISO-BET                    
000539     MOVE NOO                     TO SHIP-FLFARLIG                        
000540     MOVE SPACES                  TO SHIP-KDVALISO-EXP                    
000541     MOVE ZEROES                  TO SHIP-SUORDV-EXP                      
000542                                     SHIP-SUORDV-FAKT                     
000543                                     SHIP-VKORDBTO-FAKT                   
000544                                     SHIP-VLORDBTO-FAKT                   
000545     .                                                                    
000546     EJECT                                                                
000547                                                                          
000548 S201-CREATE-BILL-SEG SECTION.                                            
000549                                                                          
000550     MOVE SHNO-IDSHIPM            TO BILL-IDSHIPM                         
000551                                     W-IDSHIPM                            
000552     MOVE VORD-IDDC               TO BILL-IDDC                            
000553     MOVE W-IDLANDX2              TO BILL-IDLANDX3-SEND                   
000554     MOVE VORD-IDLEVNR            TO BILL-IDLEVNR                         
000555     EVALUATE TRUE                                                        
000556     WHEN VORD-KDFAKTYP = 'R' OR 'G'                                      
000557       MOVE 'INV'                       TO BILL-KDFINDOC                  
000558     WHEN VORD-KDFAKTYP = 'K' OR 'N'                                      
000559       MOVE 'INT'                       TO BILL-KDFINDOC                  
000560     END-EVALUATE                                                         
000561                                                                          
000562     MOVE FUNCTION CURRENT-DATE(3:6)                                      
000563                                  TO BILL-TISKEPPN                        
000564     MOVE FUNCTION CURRENT-DATE(9:6)                                      
000565                                  TO BILL-TISKPTID                        
000566     MOVE SPACE                   TO BILL-IDDC-EXP                        
000567     .                                                                    
000568     EJECT                                                                
000569                                                                          
000570 S111-CREATE-SHIP-GMT-SEG SECTION.                                        
000571                                                                          
000572     INITIALIZE SGMT-WDE111                                               
000573     MOVE VORD-IDDISTR            TO SGMT-IDDISTR                         
000574                                     W-WDE111-IDDISTR                     
000575     MOVE VORD-IDKUNDNR           TO SGMT-IDKUNDNR                        
000576                                     W-WDE111-IDKUNDNR                    
000577     MOVE 'N'                     TO SGMT-FLCOD                           
000578     MOVE VORD-IDDC               TO SGMT-IDDC                            
000579     MOVE SPACE                   TO SGMT-IDPARTNR                        
000580     MOVE ZERO                    TO SGMT-KDFKBIL                         
000581                                     SGMT-KDFORSKN                        
000582     MOVE -1                      TO SGMT-KDLEVVIL                        
000583     MOVE 9                       TO SGMT-KDORDKL-MAX                     
000584     MOVE  SPACE                  TO  SGMT-KDVALISO                       
000585     MOVE  ZERO                   TO  SGMT-PRKURS                         
000586     COMPUTE SGMT-TISKEPPN-9KOMPL = 9999999 -                             
000587                                    SHIP-TISKEPPN                         
000588     .                                                                    
000589     EJECT                                                                
000590                                                                          
000591 S211-CREATE-BILL-GMT-SEG SECTION.                                        
000592                                                                          
000593     INITIALIZE BGMT-WDE211                                               
000594     MOVE VORD-IDDISTR             TO BGMT-IDDISTR                        
000595                                      W-WDE211-IDDISTR                    
000596     MOVE VORD-IDKUNDNR            TO BGMT-IDKUNDNR                       
000597                                      W-WDE211-IDKUNDNR                   
000598     MOVE SPACE                    TO BGMT-IDPARTNR                       
000599     MOVE 'N'                      TO BGMT-FLCOD                          
000600                                      BGMT-FLSEPINV                       
000601     MOVE ZERO                     TO BGMT-KDLEVVIL                       
000602     MOVE ZERO                     TO BGMT-PRAVDRAG                       
000603     MOVE ZERO                     TO BGMT-PREMBHNT                       
000604     MOVE ZERO                     TO BGMT-PRFOERS                        
000605     MOVE ZERO                     TO BGMT-PRFRAKT                        
000606     MOVE ZERO                     TO BGMT-PRLEGKST                       
000607     MOVE ZERO                     TO BGMT-REAVDRAG                       
000608     MOVE ZERO                     TO BGMT-REEMBHNT                       
000609     MOVE ZERO                     TO BGMT-REFOERS                        
000610     MOVE ZERO                     TO BGMT-RELEGKST                       
000611     MOVE ZERO                     TO BGMT-REOVKOFF                       
000612     IF DIST38-FAKTURA-PER-ORDER                                          
000613        MOVE YES                   TO BGMT-FLSEPINV                       
000614     END-IF                                                               
000615     .                                                                    
000616     EJECT                                                                
000617                                                                          
000618 S121-CREATE-SHIP-KOL-SEG SECTION.                                        
000619                                                                          
000620     INITIALIZE SKOLLI-WDE121                                             
000621                                                                          
000622                                                                          
000623     MOVE VORD-IDPRODNR            TO SKOLLI-IDPRODNR                     
000624                                      W-IDPRODNR                          
000625     MOVE KOLLI-IDKOLLI            TO SKOLLI-IDKOLLI                      
000626                                      W-IDKOLLI                           
000627     MOVE KOLLI-IDKOLLI-SAMP       TO SKOLLI-IDKOLLI-SAMP                 
000628     MOVE ZERO                     TO SKOLLI-DIKOLLIB                     
000629                                      SKOLLI-DIKOLLIH                     
000630                                      SKOLLI-DIKOLLIL                     
000631     MOVE SPACE                    TO SKOLLI-FLDIRLEV                     
000632     MOVE ZERO                     TO SKOLLI-IDORDER                      
000633     MOVE -1                       TO SKOLLI-KDEMBTYP                     
000634                                      SKOLLI-KDFRAKT                      
000635                                      SKOLLI-KDFARLIG-KOLLI               
000636     MOVE SPACE                    TO SKOLLI-KDKOLLI                      
000637     MOVE ZERO                     TO SKOLLI-KVFLAMP-KOLLI                
000638     MOVE ZERO                     TO SKOLLI-SUORDV-LOC                   
000639                                      SKOLLI-SUORDV-LOCPREL               
000640     MOVE SPACE                    TO SKOLLI-KDVALISO                     
000641                                      SKOLLI-KDVALISO-EXP                 
000642     MOVE ZERO                     TO SKOLLI-SUORDV                       
000643                                      SKOLLI-SUORDV-EXP                   
000644                                      SKOLLI-TIPACKN                      
000645                                      SKOLLI-VKORDBTO-KOLLI               
000646                                      SKOLLI-VKORDNTO-KOLLI               
000647                                      SKOLLI-VLORDBTO-KOLLI               
000648     MOVE ZERO                     TO SKOLLI-IDDISTR                      
000649                                      SKOLLI-IDKUNDNR                     
000650     MOVE SPACE                    TO SKOLLI-IDKUNDRF                     
000651     MOVE -1                       TO SKOLLI-KDORDKL                      
000652     MOVE SPACE                    TO SKOLLI-KDFAKTYP                     
000653                                      SKOLLI-FLCROSS                      
000654     MOVE ZERO                     TO SKOLLI-TIORDREG                     
000655                                      SKOLLI-IDFAKT                       
000656                                      SKOLLI-IDFAKT-EXP                   
000657     .                                                                    
000658     EJECT                                                                
000659                                                                          
000660 S221-CREATE-BILL-KOL-SEG SECTION.                                        
000661                                                                          
000662     INITIALIZE  BKOLLI-WDE221                                            
000663                                                                          
000664     MOVE VORD-IDPRODNR            TO BKOLLI-IDPRODNR                     
000665                                      W-IDPRODNR                          
000666     MOVE KOLLI-IDKOLLI            TO BKOLLI-IDKOLLI                      
000667                                      W-IDKOLLI                           
000668     MOVE SPACE                    TO BGMT-IDPARTNR                       
000669     MOVE 'N'                      TO BGMT-FLCOD                          
000670     MOVE ZERO                     TO BKOLLI-IDDISTR                      
000671                                      BKOLLI-IDKUNDNR                     
000672     MOVE SPACE                    TO BKOLLI-IDKUNDRF                     
000673                                      BKOLLI-FLOVRLEV                     
000674                                      BKOLLI-KDFAKTYP                     
000675                                      BKOLLI-BEKUNDRF                     
000676     MOVE -1                       TO BKOLLI-KDORDKL                      
000677     MOVE SPACE                    TO BKOLLI-KDPRSTA                      
000678                                      BKOLLI-FLCROSS                      
000679     MOVE ZERO                     TO BKOLLI-VKORDBTO-KOLLI               
000680                                                                          
000681     .                                                                    
000682     EJECT                                                                
000683                                                                          
000684 Z-FINIT SECTION.                                                         
000685                                                                          
000686     CLOSE   W476BMP                                                      
000687     .                                                                    
000688     EJECT                                                                
000689 X-TAKE-CHECKPOINT   SECTION.                                             
000690                                                                          
000691* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
000692* --- SAVE DATABASE KEYS IF NECESSARY                                     
000693     PERFORM IMS-CHECKPOINT                                               
000694     MOVE ZERO TO CHKP-ANT                                                
000695* --- REREAD DATABASE IF NECESSARY                                        
000696     .                                                                    
000697     EJECT                                                                
000698                                                                          
000699                                                                          
000700* --- IMS SECTIONS  ---                                                   
000701                                                                          
000702     EJECT                                                                
000703 IMS-AUTFAKT-GET-ROTSEG SECTION.                                          
000704     SKIP1                                                                
000705     STRING 'WDG701  (WDGXKEY  =' W-4726-X  ')'                           
000706          DELIMITED BY SIZE INTO SSA1                                     
000707     MOVE '  GE'              TO GOOD-STATUSCODES                         
000708     CALL CBLTDLI USING GU  4726-PCB DLI-IO-WDGX01 SSA1                   
000709     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000710     PERFORM IMS-STATUSCHECK                                              
000711     .                                                                    
000712     SKIP3                                                                
000713 IMS-AUTFAKT-GNP-DISTSEG-OKVAL SECTION.                                   
000714     SKIP1                                                                
000715     MOVE 'WDG717  '          TO SSA1                                     
000716     MOVE '  GE'              TO GOOD-STATUSCODES                         
000717     CALL CBLTDLI USING GNP 4726-PCB DLI-IO-WDGX4726 SSA1                 
000718     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000719     PERFORM IMS-STATUSCHECK                                              
000720     .                                                                    
000721     EJECT                                                                
000722 IMS-AUTFAKT-GNP-DISTSEG SECTION.                                         
000723     SKIP1                                                                
000724     STRING 'WDG717  *F(WDGXKEY >=' W-WDGXKEY-X ')'                       
000725          DELIMITED BY SIZE INTO SSA1                                     
000726     MOVE '  GE'              TO GOOD-STATUSCODES                         
000727     CALL CBLTDLI USING GNP 4726-PCB DLI-IO-WDGX4726 SSA1                 
000728     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000729     PERFORM IMS-STATUSCHECK                                              
000730     .                                                                    
000731     EJECT                                                                
000732 IMS-AUTFAKT-GHNP-DISTSEG SECTION.                                        
000733     SKIP1                                                                
000734     STRING 'WDG717  *F(WDGXKEY  =' W-WDGXKEY-X ')'                       
000735          DELIMITED BY SIZE INTO SSA1                                     
000736     MOVE '  GE'                TO GOOD-STATUSCODES                       
000737     CALL CBLTDLI USING GHNP                                              
000738                        4726-PCB                                          
000739                        DLI-IO-WDGX4726                                   
000740                        SSA1                                              
000741     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000742     PERFORM IMS-STATUSCHECK                                              
000743     .                                                                    
000744 IMS-AUTFAKT-DLET-DISTSEG SECTION.                                        
000745     SKIP1                                                                
000746     MOVE '  '                TO GOOD-STATUSCODES                         
000747     CALL CBLTDLI USING DLET 4726-PCB DLI-IO-WDGX4726                     
000748     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000749     PERFORM IMS-STATUSCHECK                                              
000750     .                                                                    
000751     EJECT                                                                
000752 IMS-AUTFAKT-GNP-ORDERSEG SECTION.                                        
000753     SKIP1                                                                
000754     STRING 'WDG717  (WDGXKEY  =' W-WDGXKEY-X ')'                         
000755          DELIMITED BY SIZE INTO SSA1                                     
000756     MOVE 'WDG718  '          TO SSA2                                     
000757     MOVE '  GE'              TO GOOD-STATUSCODES                         
000758     CALL CBLTDLI USING GNP                                               
000759                        4726-PCB                                          
000760                        DLI-IO-WDGX4727                                   
000761                        SSA1                                              
000762                        SSA2                                              
000763     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
000764     PERFORM IMS-STATUSCHECK                                              
000765     .                                                                    
000766     SKIP3                                                                
000767 IMS-GET-WDE601 SECTION.                                                  
000768                                                                          
000769     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
000770          DELIMITED BY SIZE INTO SSA1                                     
000771     MOVE '  GE' TO GOOD-STATUSCODES                                      
000772     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
000773     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000774     PERFORM IMS-STATUSCHECK                                              
000775     .                                                                    
000776     SKIP3                                                                
000777 IMS-GHU-WDE601 SECTION.                                                  
000778                                                                          
000779     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
000780          DELIMITED BY SIZE INTO SSA1                                     
000781     MOVE '  GE' TO GOOD-STATUSCODES                                      
000782     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
000783     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000784     PERFORM IMS-STATUSCHECK                                              
000785     .                                                                    
000786     SKIP3                                                                
000787 IMS-REPL-WDE601 SECTION.                                                 
000788                                                                          
000789     MOVE '  ' TO GOOD-STATUSCODES                                        
000790     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
000791     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000792     PERFORM IMS-STATUSCHECK                                              
000793     .                                                                    
000794     EJECT                                                                
000795 IMS-GHNP-WDE611 SECTION.                                                 
000796                                                                          
000797     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
000798          DELIMITED BY SIZE INTO SSA1                                     
000799     MOVE 'WDE611  ' TO SSA2                                              
000800     MOVE '  GE' TO GOOD-STATUSCODES                                      
000801     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
000802     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000803     PERFORM IMS-STATUSCHECK                                              
000804     .                                                                    
000805     SKIP3                                                                
000806 IMS-REPL-WDE611 SECTION.                                                 
000807                                                                          
000808     MOVE '  ' TO GOOD-STATUSCODES                                        
000809     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
000810     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000811     PERFORM IMS-STATUSCHECK                                              
000812     .                                                                    
000813     EJECT                                                                
000814 IMS-ISRT-WDE101 SECTION.                                                 
000815                                                                          
000816     MOVE 'IMS-ISRT-WDE101' TO ERROR-TEXT                                 
000817     MOVE 'WDE101  ' TO SSA1                                              
000818     MOVE '  II' TO GOOD-STATUSCODES                                      
000819     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
000820     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000821     PERFORM IMS-STATUSCHECK                                              
000822     .                                                                    
000823     EJECT                                                                
000824 IMS-ISRT-WDE111 SECTION.                                                 
000825                                                                          
000826     MOVE 'IMS-ISRT-WDE111' TO ERROR-TEXT                                 
000827     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000828          DELIMITED BY SIZE INTO SSA1                                     
000829     MOVE 'WDE111  ' TO SSA2                                              
000830     MOVE '  II' TO GOOD-STATUSCODES                                      
000831     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
000832     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000833     PERFORM IMS-STATUSCHECK                                              
000834     .                                                                    
000835     EJECT                                                                
000836 IMS-ISRT-WDE121 SECTION.                                                 
000837                                                                          
000838     MOVE 'IMS-ISRT-WDE121' TO ERROR-TEXT                                 
000839     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000840          DELIMITED BY SIZE INTO SSA1                                     
000841     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
000842          DELIMITED BY SIZE INTO SSA2                                     
000843     MOVE 'WDE121  ' TO SSA3                                              
000844     MOVE '  II' TO GOOD-STATUSCODES                                      
000845     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
000846     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000847     PERFORM IMS-STATUSCHECK                                              
000848     .                                                                    
000849     EJECT                                                                
000850                                                                          
000851 IMS-ISRT-WDE201 SECTION.                                                 
000852                                                                          
000853     MOVE 'WDE201  ' TO SSA1                                              
000854     MOVE '  II' TO GOOD-STATUSCODES                                      
000855     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
000856     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
000857     PERFORM IMS-STATUSCHECK                                              
000858     .                                                                    
000859     EJECT                                                                
000860 IMS-ISRT-WDE211 SECTION.                                                 
000861                                                                          
000862     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000863          DELIMITED BY SIZE INTO SSA1                                     
000864     MOVE 'WDE211  ' TO SSA2                                              
000865     MOVE '  II' TO GOOD-STATUSCODES                                      
000866     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
000867     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
000868     PERFORM IMS-STATUSCHECK                                              
000869     .                                                                    
000870     EJECT                                                                
000871 IMS-ISRT-WDE221 SECTION.                                                 
000872                                                                          
000873     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000874          DELIMITED BY SIZE INTO SSA1                                     
000875     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
000876          DELIMITED BY SIZE INTO SSA2                                     
000877     MOVE 'WDE221  ' TO SSA3                                              
000878     MOVE '  II' TO GOOD-STATUSCODES                                      
000879     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
000880     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
000881     PERFORM IMS-STATUSCHECK                                              
000882     .                                                                    
000883     EJECT                                                                
000884                                                                          
000885 IMS-RESTART SECTION.                                                     
000886     SKIP2                                                                
000887     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000888     MOVE '  ' TO GOOD-STATUSCODES                                        
000889     CALL CBLTDLI USING XRST MSG-PCB                                      
000890                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000891                        CHKP-AREA-LENGTH CHKP-AREA                        
000892     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000893     PERFORM IMS-STATUSCHECK                                              
000894     .                                                                    
000895     SKIP3                                                                
000896                                                                          
000897 IMS-CHECKPOINT SECTION.                                                  
000898     SKIP2                                                                
000899     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000900     MOVE '  XD' TO GOOD-STATUSCODES                                      
000901     CALL CBLTDLI USING CHKP MSG-PCB                                      
000902                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000903                        CHKP-AREA-LENGTH CHKP-AREA                        
000904     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000905     PERFORM IMS-STATUSCHECK                                              
000906                                                                          
000907     IF IMS-NOT-OK                                                        
000908       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
000909       DISPLAY ERRTEXT                                                    
000910       CALL FELLOG                                                        
000911     END-IF                                                               
000912     .                                                                    
000913     EJECT                                                                
000914 IMS-GU-WDB601    SECTION.                                                
000915     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000916          DELIMITED BY SIZE INTO SSA1                                     
000917     MOVE '  GE' TO GOOD-STATUSCODES                                      
000918     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000919     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000920     PERFORM IMS-STATUSCHECK                                              
000921     IF SEGMENT-MISSING                                                   
000922         MOVE SPACE TO DCS-KDDC                                           
000923                       DCS-IDLANDX2                                       
000924     END-IF                                                               
000925     .                                                                    
000926 IMS-STATUSCHECK SECTION.                                                 
000927     SKIP2                                                                
000928     SET STATUS-IX TO 1                                                   
000929     SEARCH GOOD-STATUS                                                   
000930       AT END                                                             
000931         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000932           DELIMITED BY SIZE INTO ERRTEXT                                 
000933         DISPLAY ERRTEXT                                                  
000934         CALL FELLOG                                                      
000935       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
000936         CONTINUE                                                         
000937     END-SEARCH                                                           
000940     .                                                                    
