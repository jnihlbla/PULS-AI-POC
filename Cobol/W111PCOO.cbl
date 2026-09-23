000001*COMPOPT STDSUB=YES                                                       
000002 ID DIVISION.                                                             
000003                                                                          
000004 PROGRAM-ID.     W111PCOO.                                                
000005 AUTHOR.         ANDERS HENRIKSSON                                        
000006 DATE-WRITTEN.   2016-07-21                                               
000007 DATE-COMPILED.                                                           
000008*    FUNKTION:                                                            
000009*        TAR FRAM OM EN ARTIKEL ÄR PREFERENTIAL COUNTRY OF ORIGIN         
000010*                                                                         
000011*    ABENDKODER:                                                          
000012*        U0016 -  . . . .                                                 
000013*        U1000 -  . . . .                                                 
000014*                                                                         
000015                                                                          
000016     EJECT                                                                
000017 ENVIRONMENT DIVISION.                                                    
000018     SKIP2                                                                
000019 INPUT-OUTPUT SECTION.                                                    
000020                                                                          
000021 FILE-CONTROL.                                                            
000022     SKIP2                                                                
000023 DATA DIVISION.                                                           
000024     SKIP2                                                                
000025 FILE SECTION.                                                            
000026     EJECT                                                                
000027 WORKING-STORAGE SECTION.                                                 
000028                                                                          
000029                                                                          
000030*    -- CHECKED BY WY2000                                                 
000031 77  IDPGM                       PIC X(8)    VALUE 'W111PCOO'.            
000032 77  JA                          PIC X       VALUE 'J'.                   
000033 77  NEJ                         PIC X       VALUE 'N'.                   
000034                                                                          
000035 77  AGRE-SW                     PIC X       VALUE 'N'.                   
000036     88  AGRE-FOUND                          VALUE 'J'.                   
000037     88  AGRE-MISSING                        VALUE 'N'.                   
000038                                                                          
000039 77  PCOO-SW                     PIC X       VALUE 'N'.                   
000040     88  PCOO-FOUND                          VALUE 'J'.                   
000041     88  PCOO-MISSING                        VALUE 'N'.                   
000042                                                                          
000043 01  W-ARBETS-AREOR.                                                      
000044     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
000045     03  WS-BEAVTAL              PIC X(10)   VALUE SPACE.                 
000046                                                                          
000047     EJECT                                                                
000048 01  DYNAMISKA-SUBPROGRAM.                                                
000049     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000050     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000060     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000061     SKIP2                                                                
000062                                                                          
000063*    --- PARAMETRAR TILL ABEND                                            
000064 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000065 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000066     SKIP2                                                                
000067 01  FELTEXT.                                                             
000068     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000069     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000070     EJECT                                                                
000071                                                                          
000072*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000073*                                                                         
000074 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000075                                                                          
000076 01  NYCKLAR-TILL-DLI.                                                    
000077     03  W-WDM101KY-X.                                                    
000078         05  W-IDARTNR-WDM1      PIC S9(9)   VALUE ZERO COMP-3.           
000079         05  W-IDLEVNR-WDM1      PIC X(5)    VALUE SPACE.                 
000080                                                                          
000090     03  W-WDGXKEY01-X.                                                   
000091         05  W-WDGXKEY01.                                                 
000092             07  W-IDHTYP        PIC X(4)    VALUE '5109'.                
000093             07  W-FILLER        PIC X(26)   VALUE LOW-VALUE.             
000094                                                                          
000095     03  W-BEAVTAL-X.                                                     
000096         05  W-BEAVTAL           PIC X(10)   VALUE SPACE.                 
000097                                                                          
000098     03  W-IDLANDX2-X.                                                    
000099         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
000100                                                                          
000101     SKIP2                                                                
000102*    --- STATUS-KOD FRÅN IMS                                              
000103 01  STATUS-WS                   PIC XX.                                  
000104     88  SEGMENT-FINNS                       VALUE '  '.                  
000105     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000106     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000107     SKIP2                                                                
000108 01  GODK-STATUSKODER.                                                    
000109     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000110     SKIP3                                                                
000111 01  SSA1                        PIC X(64).                               
000112 01  SSA2                        PIC X(64).                               
000113 01  SSA3                        PIC X(64).                               
000114     EJECT                                                                
000115*    --- IMS FUNKTIONSKODER                                               
000116*01  -COPY W0003                                                          
000117     EJECT                                                                
000118*    ---  DLI INPUT-OUTPUT AREA                                           
000119 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000120     EJECT                                                                
000121 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM101'.                      
000122 01  DLI-IO-WDM101.                                                       
000123*    03  -COPY WDM101                                                     
000124     EJECT                                                                
000125 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM111'.                      
000126 01  DLI-IO-WDM111.                                                       
000127*    03  -COPY WDM111                                                     
000128     EJECT                                                                
000129 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM121'.                      
000130 01  DLI-IO-WDM121.                                                       
000131*    03  -COPY WDM121                                                     
000132     EJECT                                                                
000133 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
000134 01  DLI-IO-WDGX01.                                                       
000135*    03  -COPY WDGX01                                                     
000136     EJECT                                                                
000137 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5110'.                    
000138 01  DLI-IO-WDGX5110.                                                     
000139*    03  -COPY WDGX5110                                                   
000140     EJECT                                                                
000141 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5112'.                    
000142 01  DLI-IO-WDGX5112.                                                     
000143*    03  -COPY WDGX5112                                                   
000144     EJECT                                                                
000145                                                                          
000146 LINKAGE SECTION.                                                         
000147     SKIP2                                                                
000148 01  W111PCOO-AREA.                                                       
000149*    03  -COPY W111PCOO                                                   
000150     EJECT                                                                
000151*01  -COPY W0008  -PRE WDM1-                                              
000152     05  FILLER                  PIC X.                                   
000153     EJECT                                                                
000154*01  -COPY W0008  -PRE WDR2-                                              
000155     05  FILLER                  PIC X(30).                               
000156     05  KFA-BEAVTAL             PIC X(10).                               
000157     EJECT                                                                
000158                                                                          
000159 PROCEDURE DIVISION  USING W111PCOO-AREA  WDM1-PCB WDR2-PCB.              
000160 MAIN SECTION.                                                            
000161     ENTRY 'DLITCBL' USING W111PCOO-AREA  WDM1-PCB WDR2-PCB.              
000162                                                                          
000163     ACCEPT DAGENS-DATUM  FROM DATE                                       
000164                                                                          
000165     EVALUATE PCOO-KDCALL                                                 
000166                                                                          
000167       WHEN 001                                                           
000168          PERFORM A-GET-AGREEMENT                                         
000169          PERFORM B-GET-PCOO                                              
000170       WHEN OTHER                                                         
000171***              FEL KOD PÅ KDCALL (KDCALL) FINNS EJ                      
000172          MOVE '2'        TO PCOO-KDSVAR                                  
000173     END-EVALUATE                                                         
000174                                                                          
000175     MOVE ZERO TO RETURN-CODE                                             
000176     GOBACK                                                               
000177     .                                                                    
000178     EJECT                                                                
000179                                                                          
000180 A-GET-AGREEMENT SECTION.                                                 
000181     MOVE JA  TO PCOO-FLPCOO                                              
000182     MOVE NEJ TO AGRE-SW                                                  
000183     PERFORM IMS-GU-WDR201                                                
000184     MOVE PCOO-IDLANDX2 TO W-IDLANDX2                                     
000185     PERFORM IMS-GNP-WDGX5112                                             
000186     IF SEGMENT-FINNS                                                     
000187       MOVE JA TO AGRE-SW                                                 
000188       MOVE KFA-BEAVTAL TO WS-BEAVTAL                                     
000189     END-IF                                                               
000190                                                                          
000200     MOVE '0'      TO PCOO-KDSVAR                                         
000210     .                                                                    
000211     EJECT                                                                
000212                                                                          
000213 B-GET-PCOO      SECTION.                                                 
000214     MOVE NEJ TO PCOO-SW                                                  
000215     IF AGRE-FOUND                                                        
000216       MOVE PCOO-IDARTNR         TO W-IDARTNR-WDM1                        
000217       MOVE PCOO-IDLEVNR         TO W-IDLEVNR-WDM1                        
000218       MOVE WS-BEAVTAL           TO W-BEAVTAL                             
000219       PERFORM IMS-GU-WDM111                                              
000220       IF SEGMENT-FINNS                                                   
000221         PERFORM IMS-GNP-WDM121                                           
000222         PERFORM UNTIL SEGMENT-SAKNAS OR PCOO-FOUND                       
000223           IF  DAGENS-DATUM > STAV-TIGILTIG-FOM                           
000224           AND DAGENS-DATUM < STAV-TIGILTIG-TOM                           
000225             IF STAV-IDSTAMIC = 1                                         
000226               MOVE NEJ TO PCOO-FLPCOO                                    
000227               MOVE JA TO PCOO-SW                                         
000228             ELSE                                                         
000229               MOVE JA  TO PCOO-FLPCOO                                    
000230               MOVE JA TO PCOO-SW                                         
000231             END-IF                                                       
000232           ELSE                                                           
000233             MOVE JA  TO PCOO-FLPCOO                                      
000234           END-IF                                                         
000235           PERFORM IMS-GNP-WDM121                                         
000236         END-PERFORM                                                      
000237       ELSE                                                               
000238         MOVE JA  TO PCOO-FLPCOO                                          
000239       END-IF                                                             
000240     ELSE                                                                 
000241       MOVE JA  TO PCOO-FLPCOO                                            
000242     END-IF                                                               
000243     MOVE '0'      TO PCOO-KDSVAR                                         
000244     .                                                                    
000245     EJECT                                                                
000246                                                                          
000247                                                                          
000248* --- IMS SEKTIONER ---                                                   
000249     SKIP3                                                                
000250                                                                          
000260 IMS-GU-WDM111 SECTION.                                                   
000270     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
000280          DELIMITED BY SIZE INTO SSA1                                     
000290     STRING 'WDM111  (BEAVTAL  =' W-BEAVTAL-X ')'                         
000300          DELIMITED BY SIZE INTO SSA2                                     
000400     MOVE '  GE'                 TO GODK-STATUSKODER                      
000500     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM111 SSA1 SSA2               
000600     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
000700     PERFORM IMS-STATUSKONTROLL                                           
000710     .                                                                    
000720     SKIP3                                                                
000730                                                                          
000731 IMS-GNP-WDM121 SECTION.                                                  
000732     MOVE   'WDM121 ' TO SSA1                                             
000733     MOVE '  GE'                 TO GODK-STATUSKODER                      
000734     CALL CBLTDLI USING GNP WDM1-PCB DLI-IO-WDM121 SSA1                   
000735     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
000736     PERFORM IMS-STATUSKONTROLL                                           
000737     .                                                                    
000738     SKIP3                                                                
000739                                                                          
000740 IMS-GU-WDR201   SECTION.                                                 
000741     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY01-X ')'                       
000742          DELIMITED BY SIZE INTO SSA1                                     
000743     MOVE '  GE'                 TO GODK-STATUSKODER                      
000744     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX01 SSA1                    
000745     MOVE WDR2-STATUS-CODE       TO STATUS-WS                             
000746     PERFORM IMS-STATUSKONTROLL                                           
000747     .                                                                    
000748     EJECT                                                                
000749                                                                          
000750 IMS-GNP-WDGX5112 SECTION.                                                
000751     STRING 'WDGX5112(IDLANDX2 =' W-IDLANDX2-X ')'                        
000752          DELIMITED BY SIZE INTO SSA1                                     
000753     MOVE '  GE'                 TO GODK-STATUSKODER                      
000754     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX5112                      
000755                        SSA1                                              
000756     MOVE WDR2-STATUS-CODE       TO STATUS-WS                             
000757     PERFORM IMS-STATUSKONTROLL                                           
000758     .                                                                    
000759     EJECT                                                                
000760                                                                          
000761 IMS-STATUSKONTROLL SECTION.                                              
000762     SET STATUS-IX TO 1                                                   
000763     SEARCH GODK-STATUS                                                   
000764       AT END                                                             
000765         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000766           DELIMITED BY SIZE INTO FELTEXT                                 
000767         DISPLAY FELTEXT                                                  
000768         CALL FELLOG                                                      
000769       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000770         CONTINUE                                                         
000771     END-SEARCH                                                           
000772     .                                                                    
