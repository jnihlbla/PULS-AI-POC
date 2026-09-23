000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5101600.                                                
000003 AUTHOR.         MARKUS ASPFJÄLL.                                         
000004 DATE-WRITTEN.   02/02/15.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNKTION:                                                            
000009*        LÄSER FILEN W51016 O UPPDATERAR ELLER LÄGGER UPP NYA             
000010*        POSTER PÅ WDB1, FILEN INNEHÅLLER KUNDREG INFO                    
000011*                                                                         
000012*        PROGRAMMET UPPDATERAR WDB1                                       
000013*                                                                         
000014                                                                          
000015     SKIP3                                                                
000016 ENVIRONMENT DIVISION.                                                    
000017     SKIP2                                                                
000018 INPUT-OUTPUT SECTION.                                                    
000019                                                                          
000020 FILE-CONTROL.                                                            
000021     SKIP2                                                                
000022*          --- INFIL W51016 MED KUNDREG INFO                              
000023     SELECT W51016                     ASSIGN TO W51016D1.                
000024     EJECT                                                                
000025 DATA DIVISION.                                                           
000026     SKIP3                                                                
000027 FILE SECTION.                                                            
000028     SKIP3                                                                
000029 FD  W51016                                                               
000030     RECORDING       F                                                    
000031     BLOCK CONTAINS  0.                                                   
000032                                                                          
000033*01  -COPY WDB101      -L.                                                
000034     EJECT                                                                
000035 WORKING-STORAGE SECTION.                                                 
000036                                                                          
000037 77  IDPGM                       PIC X(8)    VALUE 'W5101600'.            
000038 01  CHKP-VAR.                                                            
000039     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000040     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000041     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000042     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000043     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000044     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
000045 77  JA                          PIC X       VALUE 'J'.                   
000046 77  NEJ                         PIC X       VALUE 'N'.                   
000047 01  FELTEXT.                                                             
000048     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000049     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000050                                                                          
000051 77  W51016-EOF-SW               PIC X       VALUE 'N'.                   
000052     88  END-OF-W51016                       VALUE 'J'.                   
000053     EJECT                                                                
000054 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000055 01  FILLER REDEFINES DAGENS-DATUM.                                       
000056     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000057     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000058     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000059     EJECT                                                                
000060 01  DYNAMISKA-SUBPROGRAM.                                                
000061*                                                                         
000062     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000063     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000064     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000065     EJECT                                                                
000066*    --- PARAMETRAR TILL POSTSUM                                          
000067*                                                                         
000068*01  -COPY W0005   -PRE  POSTSUM-                                         
000069     EJECT                                                                
000070 01  IN-AREA-START               PIC X(24)   VALUE                        
000071                                             'IN-AREA-START'.             
000072     SKIP2                                                                
000073                                                                          
000074*01  AREA -COPY WDB101     -PRE IN-                                       
000075*                                                                         
000076     EJECT                                                                
000077 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000078     SKIP3                                                                
000079 01  NYCKLAR-TILL-DLI.                                                    
000080     03  W-WDB101KY-X.                                                    
000081         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
000082         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
000083     SKIP2                                                                
000084*    --- STATUS-KOD FRÅN IMS                                              
000085 01  STATUS-WS                   PIC XX.                                  
000086     88  SEGMENT-FINNS                       VALUE '  '.                  
000087     88  INSERT-OK                           VALUE '  '.                  
000088     88  REPL-OK                             VALUE '  '.                  
000089     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000090     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000091     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000092     88  IMS-EJ-OK                           VALUE 'XD'.                  
000093     SKIP2                                                                
000094 01  GODK-STATUSKODER.                                                    
000095     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000096     SKIP3                                                                
000097 01  SSA1                        PIC X(64).                               
000098 01  SSA2                        PIC X(64).                               
000099     EJECT                                                                
000100*    --- IMS FUNKTIONSKODER                                               
000101*01  -COPY W0003                                                          
000102     EJECT                                                                
000103*    ---  DLI INPUT-OUTPUT AREA                                           
000104                                                                          
000105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
000106 01  DLI-IO-WDB101.                                                       
000107*    03  -COPY WDB101                                                     
000108                                                                          
000109     EJECT                                                                
000110 LINKAGE SECTION.                                                         
000111                                                                          
000112*01  -COPY W0009   -PRE MSG-                                              
000113                                                                          
000114*01  -COPY W0008  -PRE WDB1-                                              
000115     05  FILLER                  PIC X.                                   
000116     EJECT                                                                
000117 PROCEDURE DIVISION  USING MSG-PCB WDB1-PCB.                              
000118 MAIN SECTION.                                                            
000119     ENTRY 'DLITCBL' USING MSG-PCB WDB1-PCB.                              
000120                                                                          
000121     PERFORM A-INIT                                                       
000122     PERFORM S01-LAES-W51016                                              
000123     PERFORM UNTIL END-OF-W51016                                          
000124       IF CHKP-ANT > CHKP-MAX                                             
000125         PERFORM X-TAG-CHECKPOINT                                         
000126       END-IF                                                             
000127                                                                          
000128       PERFORM C-UPPDAT-WDB1                                              
000129                                                                          
000130       PERFORM S01-LAES-W51016                                            
000131     END-PERFORM                                                          
000132                                                                          
000133                                                                          
000134     PERFORM Z-FINIT                                                      
000135                                                                          
000136     MOVE ZERO TO RETURN-CODE                                             
000137     GOBACK                                                               
000138     .                                                                    
000139     EJECT                                                                
000140 A-INIT SECTION.                                                          
000141     SKIP2                                                                
000142                                                                          
000143     PERFORM IMS-RESTART                                                  
000144                                                                          
000145     OPEN INPUT W51016                                                    
000146                                                                          
000147                                                                          
000148     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000149     .                                                                    
000150     EJECT                                                                
000151 C-UPPDAT-WDB1 SECTION.                                                   
000152                                                                          
000153     MOVE IN-BET-IDPARTNR     TO W-IDPARTNR                               
000154     MOVE IN-BET-IDFTG        TO W-IDFTG                                  
000155     PERFORM IMS-GET-WDB101                                               
000156     IF SEGMENT-FINNS                                                     
000157       MOVE IN-BET-IDLANDX2      TO BET-IDLANDX2                          
000158       MOVE IN-BET-IDPARTNR      TO BET-IDPARTNR                          
000159       MOVE IN-BET-IDFTG         TO BET-IDFTG                             
000160       MOVE IN-BET-IDVAT         TO BET-IDVAT                             
000161       MOVE IN-BET-KDBETVIL      TO BET-KDBETVIL                          
000162       MOVE IN-BET-KDKREDSP      TO BET-KDKREDSP                          
000163       MOVE IN-BET-KDTRADP       TO BET-KDTRADP                           
000164       MOVE IN-BET-KDVALISO      TO BET-KDVALISO                          
000165       MOVE IN-BET-TISTADAT      TO BET-TISTADAT                          
000166       MOVE IN-BET-TIUPPDAT      TO BET-TIUPPDAT                          
000167       MOVE IN-BET-TISTODAT      TO BET-TISTODAT                          
000168       MOVE IN-BET-IDUSER        TO BET-IDUSER                            
000169*** IN-BET-IDPROMR RÖRS EJ HÄR, UPPD. ENDAST I PGM W3031100               
000170***    MOVE IN-BET-IDPROMR       TO BET-IDPROMR                           
000171       MOVE IN-BET-ADBETRAD-1    TO BET-ADBETRAD-1                        
000172       MOVE IN-BET-ADBETRAD-2    TO BET-ADBETRAD-2                        
000173       MOVE IN-BET-BELAND-SVE    TO BET-BELAND-SVE                        
000174       MOVE IN-BET-BEBETRAD-1    TO BET-BEBETRAD-1                        
000175       MOVE IN-BET-BEBETRAD-2    TO BET-BEBETRAD-2                        
000176       MOVE IN-BET-BEBETVIL      TO BET-BEBETVIL                          
000177       MOVE IN-BET-FLLOCCUR      TO BET-FLLOCCUR                          
000178       MOVE IN-BET-FLRATE        TO BET-FLRATE                            
000179       MOVE IN-BET-KDVALTYP      TO BET-KDVALTYP                          
000180*** IN-BET-IDLEVNR-FIN UPD EJ HÄR. ENDAST I PGM W5013100                  
000181                                                                          
000182       PERFORM IMS-REPL-WDB101                                            
000183                                                                          
000184     ELSE                                                                 
000185       IF SEGMENT-SAKNAS                                                  
000186         MOVE IN-AREA         TO DLI-IO-WDB101                            
000187         MOVE 'N'             TO BET-FLARTRAB                             
000188                                                                          
000189         PERFORM IMS-ISRT-WDB101                                          
000190       END-IF                                                             
000191     END-IF                                                               
000192     ADD +1                   TO CHKP-ANT                                 
000193     .                                                                    
000194     EJECT                                                                
000195 Z-FINIT SECTION.                                                         
000196                                                                          
000197                                                                          
000198     CLOSE W51016                                                         
000199     SKIP2                                                                
000200     MOVE 'S' TO POSTSUM-OPKOD                                            
000201     CALL POSTSUM USING POSTSUM-PARM                                      
000202     .                                                                    
000203     EJECT                                                                
000204 S01-LAES-W51016  SECTION.                                                
000205                                                                          
000206     READ W51016 INTO IN-AREA                                             
000207     AT END                                                               
000208        SET END-OF-W51016 TO TRUE                                         
000209     NOT AT END                                                           
000210        MOVE 'W51016' TO POSTSUM-FDNAMN                                   
000211        MOVE 'W51016D1' TO POSTSUM-DDNAMN2                                
000212        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
000213        CALL POSTSUM USING POSTSUM-PARM                                   
000214     END-READ                                                             
000215     .                                                                    
000216     EJECT                                                                
000217 X-TAG-CHECKPOINT   SECTION.                                              
000218                                                                          
000219     PERFORM IMS-CHECKPOINT                                               
000220     MOVE ZERO TO CHKP-ANT                                                
000221     .                                                                    
000222                                                                          
000223 IMS-GET-WDB101 SECTION.                                                  
000224                                                                          
000225     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
000226          DELIMITED BY SIZE INTO SSA1                                     
000227     MOVE '  GE' TO GODK-STATUSKODER                                      
000228     CALL CBLTDLI USING GHU WDB1-PCB DLI-IO-WDB101 SSA1                   
000229     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000230     PERFORM IMS-STATUSKONTROLL                                           
000231     .                                                                    
000232     SKIP3                                                                
000233 IMS-ISRT-WDB101 SECTION.                                                 
000234                                                                          
000235     MOVE 'WDB101 ' TO SSA1                                               
000236     MOVE '  II' TO GODK-STATUSKODER                                      
000237     CALL CBLTDLI USING ISRT WDB1-PCB DLI-IO-WDB101 SSA1                  
000238     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000239     PERFORM IMS-STATUSKONTROLL                                           
000240                                                                          
000241     IF INSERT-OK                                                         
000242       MOVE 'ISRT'        TO POSTSUM-FDNAMN                               
000243       MOVE 'WDB1'        TO POSTSUM-DDNAMN2                              
000244       MOVE 'ISRT'        TO POSTSUM-TRANSTYP                             
000245       CALL POSTSUM USING POSTSUM-PARM                                    
000246     END-IF                                                               
000247     .                                                                    
000248     SKIP3                                                                
000249 IMS-REPL-WDB101 SECTION.                                                 
000250                                                                          
000251     MOVE '  ' TO GODK-STATUSKODER                                        
000252     CALL CBLTDLI USING REPL WDB1-PCB DLI-IO-WDB101                       
000253     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000254     PERFORM IMS-STATUSKONTROLL                                           
000255     IF INSERT-OK                                                         
000256       MOVE 'REPL'        TO POSTSUM-FDNAMN                               
000257       MOVE 'WDB1'        TO POSTSUM-DDNAMN2                              
000258       MOVE 'REPL'        TO POSTSUM-TRANSTYP                             
000259       CALL POSTSUM USING POSTSUM-PARM                                    
000260     END-IF                                                               
000261     .                                                                    
000262     EJECT                                                                
000263 IMS-RESTART SECTION.                                                     
000264     SKIP2                                                                
000265     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000266     MOVE '  ' TO GODK-STATUSKODER                                        
000267     CALL CBLTDLI USING XRST MSG-PCB                                      
000268                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000269                        CHKP-AREA-LENGTH CHKP-AREA                        
000270     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000271     PERFORM IMS-STATUSKONTROLL                                           
000272     .                                                                    
000273     SKIP3                                                                
000274 IMS-CHECKPOINT SECTION.                                                  
000275     SKIP2                                                                
000276     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000277     MOVE '  XD' TO GODK-STATUSKODER                                      
000278     CALL CBLTDLI USING CHKP MSG-PCB                                      
000279                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000280                        CHKP-AREA-LENGTH CHKP-AREA                        
000281     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000282     PERFORM IMS-STATUSKONTROLL                                           
000283                                                                          
000284     IF IMS-EJ-OK                                                         
000285       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
000286       DISPLAY FELTEXT                                                    
000287       CALL FELLOG                                                        
000288     END-IF                                                               
000289     .                                                                    
000290     EJECT                                                                
000291 IMS-STATUSKONTROLL SECTION.                                              
000292     SKIP2                                                                
000293     SET STATUS-IX TO 1                                                   
000294     SEARCH GODK-STATUS                                                   
000295       AT END                                                             
000296         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000297           DELIMITED BY SIZE INTO FELTEXT                                 
000298         DISPLAY FELTEXT                                                  
000299         CALL FELLOG                                                      
000300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000301         CONTINUE                                                         
000302     END-SEARCH                                                           
000303     .                                                                    
