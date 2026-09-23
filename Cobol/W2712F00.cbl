000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W2712F00.                                                
000004*AUTHOR.         JOHAN NIHLBLAD.                                          
000005*DATE-WRITTEN.   APRIL 2022.                                              
000006                                                                          
000007*    REMARKS                                                              
000008*                                                                         
000009*    FUNCTION:                                                            
000010*        UPDATES CHANGED REFILLINGPOINTS ON WDK7                          
000014*                                                                         
000015*        PROGRAM  UPDATES WDK7                                            
000016*                                                                         
000017*    ABENDKODER:                                                          
000018*        U0016 -  . . . .                                                 
000019*        U1000 -  . . . .                                                 
000020*                                                                         
000021                                                                          
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     SKIP2                                                                
000025 INPUT-OUTPUT SECTION.                                                    
000026                                                                          
000027 FILE-CONTROL.                                                            
000028     SKIP2                                                                
000029*          --- PROGNOS SOM SKA FÖRÄNDRAS                                  
000030     SELECT W2712F                     ASSIGN TO W2712FD1.                
000031     EJECT                                                                
000032 DATA DIVISION.                                                           
000033     SKIP3                                                                
000034 FILE SECTION.                                                            
000035     SKIP3                                                                
000036 FD  W2712F                                                               
000037     RECORDING       F                                                    
000038     BLOCK CONTAINS  0.                                                   
000039                                                                          
000040*01  -COPY W2712F      -L.                                                
000041     EJECT                                                                
000042 WORKING-STORAGE SECTION.                                                 
000043     SKIP2                                                                
000044                                                                          
000045*    -- CHECKED BY WY2000                                                 
000046 77  IDPGM                       PIC X(8)    VALUE 'W2712F00'.            
000047 77  JA                          PIC X       VALUE 'J'.                   
000048 77  NEJ                         PIC X       VALUE 'N'.                   
000049 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
000050     SKIP2                                                                
000051 01  CHKP-VAR.                                                            
000052 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
000053 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
000054 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
000055 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
000056 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
000057 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
000058                                                                          
000059                                                                          
000060                                                                          
000061 01  FELTEXT.                                                             
000062     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000063     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000064                                                                          
000065 77  W2712F-EOF-SW               PIC X       VALUE 'N'.                   
000066     88  END-OF-W2712F                       VALUE 'J'.                   
000067                                                                          
000068 77  K7-SW                       PIC X       VALUE 'J'.                   
000069     88  K7-FINNS                            VALUE 'J'.                   
000070     EJECT                                                                
000071 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000072 01  FILLER REDEFINES DAGENS-DATUM.                                       
000073     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000074     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000075     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000076     EJECT                                                                
000077 01  DYNAMISKA-SUBPROGRAM.                                                
000078*                                                                         
000079     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000080     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000081     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000082     EJECT                                                                
000083*    --- PARAMETRAR TILL POSTSUM                                          
000084*                                                                         
000085*01  -COPY W0005   -PRE  POSTSUM-                                         
000086     EJECT                                                                
000087 01  IN-AREA-START               PIC X(24)   VALUE                        
000088                                             'IN-AREA-START'.             
000089     SKIP2                                                                
000090                                                                          
000091*01  AREA -COPY W2712F     -PRE IN-                                       
000092*                                                                         
000093     EJECT                                                                
000094 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000095     SKIP3                                                                
000096 01  NYCKLAR-TILL-DLI.                                                    
000097     03  W-IDARTNR-X.                                                     
000098         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000099     03  W-IDDC-X.                                                        
000100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000101     03  W-KDSEGKEY-X.                                                    
000102         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
000103                                                                          
000104     SKIP2                                                                
000105*    --- STATUS-KOD FRÅN IMS                                              
000106 01  STATUS-WS                   PIC XX.                                  
000107     88  SEGMENT-FINNS                       VALUE '  '.                  
000108     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000109     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000110     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000111     88  IMS-EJ-OK                           VALUE 'XD'.                  
000112     SKIP2                                                                
000113 01  GODK-STATUSKODER.                                                    
000114     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000115     SKIP3                                                                
000116 01  SSA1                        PIC X(64).                               
000119     EJECT                                                                
000120*    --- IMS FUNKTIONSKODER                                               
000121*01  -COPY W0003                                                          
000122     EJECT                                                                
000123*    ---  DLI INPUT-OUTPUT AREA                                           
000124 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000125     SKIP3                                                                
000126 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
000127 01  DLI-IO-WDK701.                                                       
000128*    03  -COPY WDK701                                                     
000129     EJECT                                                                
000130                                                                          
000131 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
000132 01  DLI-IO-WDK711.                                                       
000133*    03  -COPY WDK711                                                     
000134     EJECT                                                                
000140                                                                          
000141 LINKAGE SECTION.                                                         
000142                                                                          
000143*01  -COPY W0009   -PRE MSG-                                              
000144     EJECT                                                                
000145*01  -COPY W0008  -PRE WDK7-                                              
000146     05  FILLER                  PIC X.                                   
000147     EJECT                                                                
000151 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB.                              
000152     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB.                              
000153                                                                          
000154     PERFORM A-INIT                                                       
000155     PERFORM S01-LAES-W2712F                                              
000156     PERFORM UNTIL END-OF-W2712F                                          
000157                                                                          
000158       PERFORM B-BEHANDLA-POSTER                                          
000159                                                                          
000160       PERFORM S01-LAES-W2712F                                            
000161                                                                          
000162     END-PERFORM                                                          
000163     PERFORM Z-FINIT                                                      
000164                                                                          
000165     MOVE ZERO TO RETURN-CODE                                             
000166     GOBACK                                                               
000167     .                                                                    
000168     EJECT                                                                
000169 A-INIT SECTION.                                                          
000170     SKIP2                                                                
000171                                                                          
000172     ACCEPT DAGENS-DATUM FROM DATE                                        
000173                                                                          
000174     PERFORM IMS-RESTART                                                  
000175                                                                          
000176     OPEN INPUT W2712F                                                    
000177                                                                          
000178                                                                          
000179     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000180     .                                                                    
000181     EJECT                                                                
000182 B-BEHANDLA-POSTER SECTION.                                               
000183                                                                          
000184     MOVE IN-IDARTNR TO W-IDARTNR                                         
000185     MOVE IN-IDDC    TO W-IDDC                                            
000186     IF IN-IDARTNR = WS-IDARTNR                                           
000187       CONTINUE                                                           
000188     ELSE                                                                 
000189       IF CHKP-ANT > CHKP-MAX                                             
000190         PERFORM X-TAG-CHECKPOINT                                         
000191       END-IF                                                             
000192       PERFORM IMS-GU-WDK701                                              
000193       MOVE W-IDARTNR TO WS-IDARTNR                                       
000194     END-IF                                                               
000195     PERFORM IMS-GHNP-WDK711                                              
000196                                                                          
000197     IF SLAG-KVREFPKT = IN-KVREFPKT                                       
000198     AND SLAG-KVREFBER = IN-KVREFBER                                      
000199     AND SLAG-KVREFOVL = IN-KVREFOVL                                      
000200       CONTINUE                                                           
000201     ELSE                                                                 
000202       IF SLAG-KVREFPKT NOT = IN-KVREFPKT                                 
000203         MOVE IN-KVREFPKT  TO SLAG-KVREFPKT                               
000204       END-IF                                                             
000205       IF SLAG-KVREFBER NOT = IN-KVREFBER                                 
000206         MOVE IN-KVREFBER  TO SLAG-KVREFBER                               
000207       END-IF                                                             
000208       IF SLAG-KVREFOVL NOT = IN-KVREFOVL                                 
000209         MOVE IN-KVREFOVL  TO SLAG-KVREFOVL                               
000210       END-IF                                                             
000211       IF IN-FLREFPKT = JA                                                
000212         MOVE ZERO TO SLAG-TIREFPKT                                       
000213       END-IF                                                             
000214       IF IN-FLREFPAF = JA                                                
000215         MOVE ZERO TO SLAG-TIREFPAF                                       
000216       END-IF                                                             
000217*****                                                                     
000221       PERFORM IMS-REPL-WDK711                                            
000222       ADD +1 TO CHKP-ANT                                                 
000223     END-IF                                                               
000229     .                                                                    
000230     EJECT                                                                
000231 Z-FINIT SECTION.                                                         
000232                                                                          
000233     CLOSE W2712F                                                         
000234     SKIP2                                                                
000235     MOVE 'S' TO POSTSUM-OPKOD                                            
000236     CALL POSTSUM USING POSTSUM-PARM                                      
000237                                                                          
000238     .                                                                    
000239     EJECT                                                                
000240 S01-LAES-W2712F  SECTION.                                                
000241     SKIP2                                                                
000242     READ W2712F INTO IN-AREA                                             
000243     AT END                                                               
000244        SET END-OF-W2712F TO TRUE                                         
000245                                                                          
000246     NOT AT END                                                           
000247        MOVE 'W2712F' TO POSTSUM-FDNAMN                                   
000248        MOVE 'W2712FD1' TO POSTSUM-DDNAMN2                                
000249        CALL POSTSUM USING POSTSUM-PARM                                   
000250     END-READ                                                             
000251     .                                                                    
000252     EJECT                                                                
000253 X-TAG-CHECKPOINT   SECTION.                                              
000254                                                                          
000255* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
000256* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
000257     PERFORM IMS-CHECKPOINT                                               
000258     MOVE ZERO TO CHKP-ANT                                                
000259* --- LÄS OM DATABAS OM DET BEHÖVS                                        
000260     .                                                                    
000261     EJECT                                                                
000262* --- IMS SEKTIONER ---                                                   
000263     SKIP3                                                                
000264     EJECT                                                                
000265 IMS-RESTART SECTION.                                                     
000266     SKIP2                                                                
000267     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000268     MOVE '  ' TO GODK-STATUSKODER                                        
000269     CALL CBLTDLI USING XRST MSG-PCB                                      
000270                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000271                        CHKP-AREA-LENGTH CHKP-AREA                        
000272     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000273     PERFORM IMS-STATUSKONTROLL                                           
000274     .                                                                    
000275     EJECT                                                                
000276 IMS-CHECKPOINT SECTION.                                                  
000277     SKIP2                                                                
000278     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000279     MOVE '  XD' TO GODK-STATUSKODER                                      
000280     CALL CBLTDLI USING CHKP MSG-PCB                                      
000281                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000282                        CHKP-AREA-LENGTH CHKP-AREA                        
000283     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000284     PERFORM IMS-STATUSKONTROLL                                           
000285                                                                          
000286     IF IMS-EJ-OK                                                         
000287       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
000288       DISPLAY FELTEXT                                                    
000289       CALL FELLOG                                                        
000290     END-IF                                                               
000291     .                                                                    
000292     EJECT                                                                
000293 IMS-GU-WDK701 SECTION.                                                   
000294                                                                          
000295     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000296          DELIMITED BY SIZE INTO SSA1                                     
000297     MOVE '  ' TO GODK-STATUSKODER                                        
000298     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
000299     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000300     PERFORM IMS-STATUSKONTROLL                                           
000301     .                                                                    
000302     EJECT                                                                
000303 IMS-GHNP-WDK711 SECTION.                                                 
000304                                                                          
000305     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
000306          DELIMITED BY SIZE INTO SSA1                                     
000307     MOVE '  ' TO GODK-STATUSKODER                                        
000308     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
000309     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000310     PERFORM IMS-STATUSKONTROLL                                           
000311     .                                                                    
000312     SKIP3                                                                
000327 IMS-REPL-WDK711 SECTION.                                                 
000328                                                                          
000329     MOVE '  ' TO GODK-STATUSKODER                                        
000330     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
000331     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000332     PERFORM IMS-STATUSKONTROLL                                           
000333     .                                                                    
000334     EJECT                                                                
000343 IMS-STATUSKONTROLL SECTION.                                              
000344     SKIP2                                                                
000345     SET STATUS-IX TO 1                                                   
000346     SEARCH GODK-STATUS                                                   
000347       AT END                                                             
000348         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
000349         DISPLAY FELTEXT                                                  
000350         CALL FELLOG                                                      
000351       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000352         CONTINUE                                                         
000353     END-SEARCH                                                           
000354     .                                                                    
