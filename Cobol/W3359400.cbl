000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3359400.                                                
000003 AUTHOR.         GAVIN SMITH.                                             
000004 DATE-WRITTEN.   02/01/16.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        BMP FÖR ATT SÄNDA OM ALLA PRISFRÅGOR SOM INTE                    
000009*        HAR FÅTT NÅGOT SVAR FRÅN VIPS INOM 1 TIMME, OCH DE SOM           
000010*        INTE FICK SVAR MED PRIS.DENNA SB KÖRS IGÅNG AV EN                
000011*        TIDSHÅLLNINGSMODUL AV LASSI (W335S6)                             
000012*        SKICKAR ENDAST OM PRISFRÅGOR SOM EJ ÄR SVERIGE.                  
000013*        OMFRÅGOR SVERIGE TAS OM HAND I W3359E00 BUNTVIS.                 
000014*                                                                         
000015*        PROGRAMMET LÄSER WDC7 MED SB.                                    
000016*                         WDR4 MED DL1.                                   
000017*                                                                         
000018*    UTDATA.                                                              
000019*        SÄNDNING VIA WZ01  TILL VIPS                                     
000020*                                                                         
000021*    CHANGE LOG: E'TRACKER 5978507 DATED 071210                           
000022*                                                                         
000023                                                                          
000024     SKIP3                                                                
000025 ENVIRONMENT DIVISION.                                                    
000026     EJECT                                                                
000027 DATA DIVISION.                                                           
000028     EJECT                                                                
000029 WORKING-STORAGE SECTION.                                                 
000030 77  IDPGM                       PIC X(08)   VALUE 'W3359400'.            
000031*                                                                         
000032*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000033 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000034                                                                          
000035 77  JA                          PIC X       VALUE 'J'.                   
000036 77  NEJ                         PIC X       VALUE 'N'.                   
000037                                                                          
000038                                                                          
000039 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000040     88  NYCKLAR-OK                          VALUE 'J'.                   
000041     88  NYCKLAR-FEL                         VALUE 'N'.                   
000042     EJECT                                                                
000043 01  W-DADATTID                 PIC 9(14)    VALUE ZERO.                  
000044 01  W-TIDDIFF                  PIC 9(14)    VALUE ZERO.                  
000045*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000046 01  GENERELLA-SUBPROGRAM.                                                
000047     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000048     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000049     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000050     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
000051     EJECT                                                                
000052 01  MESSAGE-CODES.                                                       
000053     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000054     EJECT                                                                
000055 01  COUNTERS.                                                            
000056     03  TALLY-OK                PIC 9(4)    VALUE ZERO.                  
000057     03  TALLY-FEL               PIC 9(4)    VALUE ZERO.                  
000058     03  W-FIRST-TIME            PIC 9       VALUE ZERO.                  
000059*    --- AREOR FÖR KOMMUNIKATION                                          
000060 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
000061*01  -COPY WZ01SEND                                                       
000062 01  SEND-DATA.                                                           
000063*03  -COPY WZ01REQU -PRE MID-                                             
000064*03  -COPY W30391O1                                                       
000065 01   KDRC-DISPLAY               PIC X(4).                                
000066     EJECT                                                                
000067*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000068*                                                                         
000069 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000070     SKIP3                                                                
000071 01  NYCKLAR-TILL-DLI.                                                    
000072     03  WDR401-X.                                                        
000073         05  FILLER              PIC X(4)    VALUE '3101'.                
000074         05  W-IDDISTR1          PIC 9(4)    VALUE ZERO.                  
000075         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
000076     03  W-WDC701KY-X.                                                    
000077         05  W-IDDISTR           PIC 9(4)     VALUE ZERO.                 
000078         05  W-IDKUNDNR          PIC 9(7)     VALUE ZERO.                 
000079         05  W-IDBUNDLE          PIC X(15)    VALUE SPACE.                
000080     03  W-WDC711KY-X.                                                    
000081         05  W-IDPRQUES          PIC 9(7)     VALUE ZERO.                 
000082     SKIP2                                                                
000083*    --- STATUS-KOD FRÅN IMS                                              
000084 01  STATUS-WS                   PIC XX.                                  
000085     88  SEGMENT-FINNS                       VALUE '  '.                  
000086     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000087     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000088     88  DB-SLUT                             VALUE 'GB'.                  
000089     SKIP2                                                                
000090 01  GODK-STATUSKODER.                                                    
000091     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000092     SKIP3                                                                
000093 01  SSA1                        PIC X(64).                               
000094 01  SSA2                        PIC X(64).                               
000095     EJECT                                                                
000096*    --- IMS FUNKTIONSKODER                                               
000097*01  -COPY W0003                                                          
000098     EJECT                                                                
000099*    ---  DLI INPUT-OUTPUT AREA                                           
000100                                                                          
000101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7  '.                      
000102 01  DLI-IO-WDC7.                                                         
000103*    03  -COPY WDC711 -PRE RR-                                            
000104 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
000105 01  DLI-IO-WDC701.                                                       
000106*    03  -COPY WDC701                                                     
000107     EJECT                                                                
000108 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
000109 01  DLI-IO-WDC711.                                                       
000110*    03  -COPY WDC711                                                     
000111     EJECT                                                                
000112 01  FILLER         PIC X(16) VALUE 'DLI-IO-PRFEL1'.                      
000113     EJECT                                                                
000114 01  DLI-IO-WDR401.                                                       
000115*    03  -COPY WDGX3101                                                   
000116 01  DLI-IO-WDGX3102.                                                     
000117*    03  -COPY WDGX3102                                                   
000118 01  DLI-IO-WDGX3104.                                                     
000119*    03  -COPY WDGX3104                                                   
000120 01  DLI-IO-WDGX3106.                                                     
000121*    03  -COPY WDGX3106                                                   
000122     EJECT                                                                
000123 LINKAGE SECTION.                                                         
000124*01  -COPY W0008  -PRE WDC7-                                              
000125     05  FILLER                  PIC X.                                   
000126*01  -COPY W0008  -PRE WDR4-                                              
000127     05  FILLER                  PIC X.                                   
000128     EJECT                                                                
000129 PROCEDURE DIVISION  USING WDC7-PCB WDR4-PCB.                             
000130 MAIN SECTION.                                                            
000131     ENTRY 'DLITCBL' USING WDC7-PCB WDR4-PCB.                             
000132                                                                          
000133     PERFORM A-INIT                                                       
000134     PERFORM C-CHECKBASE                                                  
000135                                                                          
000136     MOVE ZERO TO RETURN-CODE                                             
000137     GOBACK                                                               
000138     .                                                                    
000139     EJECT                                                                
000140                                                                          
000141 A-INIT SECTION.                                                          
000142                                                                          
000143     MOVE FUNCTION CURRENT-DATE(1:14) TO W-DADATTID                       
000144     CONTINUE                                                             
000145     .                                                                    
000146     EJECT                                                                
000147                                                                          
000148 C-CHECKBASE SECTION.                                                     
000149                                                                          
000150     PERFORM IMS-GN-WDC7                                                  
000151     PERFORM UNTIL DB-SLUT                                                
000152****  CHECKSIF WDC701 SEGMENT***************                              
000153      IF WDC7-SEG-LEVEL = '01'                                            
000154        MOVE DLI-IO-WDC7 TO DLI-IO-WDC701                                 
000155        IF PRQ-IDDISTR = 0778 OR 8857 OR 8859                             
000156           CONTINUE                                                       
000157        ELSE                                                              
000158****  SET FIRST TIME SWITCH TO ZERO                                       
000159          IF W-FIRST-TIME = 1                                             
000160            PERFORM S06-SEND-CLOSE                                        
000161          END-IF                                                          
000162          MOVE 0 TO W-FIRST-TIME                                          
000163          MOVE PRQ-IDDISTR   TO   W-IDDISTR1                              
000164******KOLLA ATT DISTRIKT ÄR DDI                                           
000165          PERFORM IMS-GU-WDGX3102                                         
000166          MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                     
000167          MOVE PRQ-IDDISTR   TO   MOD-IDDISTR                             
000168          MOVE PRQ-IDKUNDNR  TO   MOD-IDKUNDNR                            
000169          MOVE PRQ-IDBUNDLE  TO   MOD-IDBUNDLE                            
000170        END-IF                                                            
000171      END-IF                                                              
000172****  CHECKSIF WDC711 SEGMENT***************                              
000173      IF WDC7-SEG-LEVEL = '02'                                            
000174        IF PRQ-IDDISTR = 0778 OR 8857 OR 8859                             
000175          CONTINUE                                                        
000176        ELSE                                                              
000177          IF W-FIRST-TIME = 1                                             
000178            IF LPRQ-ADDISPABS NOT = SPACE                                 
000179               MOVE 0 TO W-FIRST-TIME                                     
000180               PERFORM S06-SEND-CLOSE                                     
000181            END-IF                                                        
000182          END-IF                                                          
000183          MOVE DLI-IO-WDC7 TO DLI-IO-WDC711                               
000184          IF  LPRQ-DADATTID-OK = 0 AND                                    
000185              LPRQ-DADATTID-SEND > 0                                      
000186                                                                          
000187            COMPUTE  W-TIDDIFF = W-DADATTID - LPRQ-DADATTID-SEND          
000188            IF W-TIDDIFF > 9900                                           
000189****CHECK THAT PRICE NOT MANUALLY UPDATED,                                
000190             IF LPRQ-KDPRSTA NOT = 'M'                                    
000191****CHECK FIRSTTIME SWITCH, IF ZERO, DO OPEN,                             
000192              IF W-FIRST-TIME = 0                                         
000193                MOVE 1 TO W-FIRST-TIME                                    
000194                PERFORM S04-SEND-OPEN                                     
000195              END-IF                                                      
000196*****************FIX TA BORT**NÄSTA RAD*************                      
000197*             IF MOD-IDDISTR = 2278                                       
000198*             IF MOD-IDKUNDNR  = 7733                                     
000199***************************************************                       
000200              MOVE LPRQ-IDPRQUES TO   MOD-IDPRQUES                        
000201              MOVE LPRQ-IDARTNR  TO   MOD-IDARTNR                         
000202              MOVE LPRQ-KDORDKL  TO   MOD-KDORDKL                         
000203              MOVE LPRQ-KVBEART  TO   MOD-KVBEART                         
000204              PERFORM S05-SEND-MESSAGE                                    
000205*****************FIX TA BORT**NÄSTA RAD*************                      
000206*             END-IF                                                      
000207*             END-IF                                                      
000208***************************************************                       
000209             END-IF                                                       
000210            END-IF                                                        
000211          END-IF                                                          
000212        END-IF                                                            
000213      END-IF                                                              
000214***********************************                                       
000215      PERFORM IMS-GN-WDC7                                                 
000216     END-PERFORM                                                          
000217     IF W-FIRST-TIME = 1                                                  
000218       PERFORM S06-SEND-CLOSE                                             
000219     END-IF                                                               
000220     .                                                                    
000221     EJECT                                                                
000222 S04-SEND-OPEN SECTION.                                                   
000223     MOVE 'OPEN' TO SEND-KDFUNC                                           
000224     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000225                         SEND-OPEN-AREA                                   
000226******** OM FEL                                                           
000227     IF SEND-KDRC  > 0                                                    
000228        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
000229        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
000230        DELIMITED BY SIZE INTO FELTEXT                                    
000231        DISPLAY FELTEXT                                                   
000232        CALL FELLOG                                                       
000233     END-IF                                                               
000234     .                                                                    
000235     EJECT                                                                
000236 S05-SEND-MESSAGE SECTION.                                                
000237                                                                          
000238     MOVE 'PUT' TO SEND-KDFUNC                                            
000239     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
000240     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000241                         SEND-KVDLEN                                      
000242                         SEND-DATA                                        
000243******** OM FEL                                                           
000244     IF SEND-KDRC  > 0                                                    
000245        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
000246        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
000247        DELIMITED BY SIZE INTO FELTEXT                                    
000248        DISPLAY FELTEXT                                                   
000249        CALL FELLOG                                                       
000250     END-IF                                                               
000251     .                                                                    
000252     EJECT                                                                
000253 S06-SEND-CLOSE SECTION.                                                  
000254                                                                          
000255       MOVE 'CLOSE' TO SEND-KDFUNC                                        
000256       CALL WZ01SEND USING SEND-CONTROL-AREA                              
000257******** OM FEL                                                           
000258     IF SEND-KDRC  > 0                                                    
000259        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
000260        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
000261        DELIMITED BY SIZE INTO FELTEXT                                    
000262        DISPLAY FELTEXT                                                   
000263        CALL FELLOG                                                       
000264     END-IF                                                               
000265     .                                                                    
000266     EJECT                                                                
000267* --- IMS SEKTIONER ---                                                   
000268     SKIP3                                                                
000269 IMS-GN-WDC7    SECTION.                                                  
000270                                                                          
000271     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000272     CALL CBLTDLI USING GN  WDC7-PCB DLI-IO-WDC7                          
000273     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000274     PERFORM IMS-STATUSKONTROLL                                           
000275     .                                                                    
000276     SKIP3                                                                
000277 IMS-GU-WDGX3102 SECTION.                                                 
000278                                                                          
000279     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
000280          DELIMITED BY SIZE INTO SSA1                                     
000281     MOVE   'WDGX3102' TO SSA2                                            
000282     MOVE '  ' TO GODK-STATUSKODER                                        
000283     CALL CBLTDLI USING GU WDR4-PCB DLI-IO-WDGX3102                       
000284                                     SSA1 SSA2                            
000285     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
000286     PERFORM IMS-STATUSKONTROLL                                           
000287     .                                                                    
000288     SKIP3                                                                
000289 IMS-STATUSKONTROLL SECTION.                                              
000290                                                                          
000291     SET STATUS-IX TO 1                                                   
000292     SEARCH GODK-STATUS                                                   
000293       AT END                                                             
000294         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000295         DELIMITED BY SIZE INTO FELTEXT                                   
000296         CALL FELLOG                                                      
000297       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000298         CONTINUE                                                         
000299     END-SEARCH                                                           
000300     .                                                                    
