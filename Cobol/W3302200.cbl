000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W3302200.                                                
000004 AUTHOR.         RONNY STENHOLM.                                          
000005 DATE-WRITTEN.   95/09/18.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        SORTERAR INFIL PÅ DISTRIKTNUMMER SÖKER DISTRIKTETS               
000010*        MARKNADSBOLAG/PRISOMRÅDE LÄGGER TILL DETTA PÅ UTFIL.             
000011*                                                                         
000012*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
000013*        PROGRAMMET LÄSER      WLBETC (WDB1)                              
000014*                                                                         
000015*    ABENDKODER:                                                          
000016*        U0016 -  . . . .                                                 
000017*        U1000 -  . . . .                                                 
000018*                                                                         
000019                                                                          
000020     SKIP3                                                                
000021 ENVIRONMENT DIVISION.                                                    
000022     SKIP2                                                                
000023 INPUT-OUTPUT SECTION.                                                    
000024                                                                          
000025 FILE-CONTROL.                                                            
000026     SKIP2                                                                
000027*          --- SEKUNDÄRREGISTRET                                          
000028     SELECT W33015                     ASSIGN TO W33022D1.                
000029     SKIP2                                                                
000030*          --- SEKUNDÄRREGISTER                                           
000031     SELECT W33026                     ASSIGN TO W33022D2.                
000032     SKIP2                                                                
000033*          --- SORTERINGSFIL                                              
000034     SELECT SORTFIL                    ASSIGN TO W33022DS.                
000035     EJECT                                                                
000036 DATA DIVISION.                                                           
000037     SKIP2                                                                
000038 FILE SECTION.                                                            
000039     SKIP3                                                                
000040 FD  W33015                                                               
000041     RECORDING       F                                                    
000042     BLOCK CONTAINS  0.                                                   
000043                                                                          
000044*01  -COPY W33014      -L.                                                
000045     SKIP3                                                                
000046 FD  W33026                                                               
000047     RECORDING       F                                                    
000048     BLOCK CONTAINS  0.                                                   
000049                                                                          
000050*01  POST -COPY W33026 -PRE  UT-  -L.                                     
000051     SKIP2                                                                
000052 SD  SORTFIL.                                                             
000053                                                                          
000054*01  POST -COPY W33014      -PRE SORT-                                    
000055     EJECT                                                                
000056 WORKING-STORAGE SECTION.                                                 
000057                                                                          
000058                                                                          
000059*    -- CHECKED BY WY2000                                                 
000060 77  IDPGM                       PIC X(8)    VALUE 'W3302200'.            
000061 77  JA                          PIC X       VALUE 'J'.                   
000062 77  NEJ                         PIC X       VALUE 'N'.                   
000063                                                                          
000064 77  W33015-EOF-SW               PIC X       VALUE 'N'.                   
000065     88  END-OF-W33015                       VALUE 'J'.                   
000066                                                                          
000067 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
000068     88  END-OF-SORTFIL                      VALUE 'J'.                   
000069     EJECT                                                                
000070 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000071 01  FILLER REDEFINES DAGENS-DATUM.                                       
000072     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000073     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000074     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000075     EJECT                                                                
000076 01  DYNAMISKA-SUBPROGRAM.                                                
000077*                                                                         
000078     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000079     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000080     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000081     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000082     SKIP2                                                                
000083*    --- PARAMETRAR TILL ABEND                                            
000084                                                                          
000085 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000086 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000087 77  SPAR-IDMARKBO               PIC X    VALUE SPACE.                    
000088 77  SPAR-IDPROMRN               PIC X(2) VALUE SPACE.                    
000089     SKIP2                                                                
000090 01  FELTEXT.                                                             
000091     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000092     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000093     EJECT                                                                
000094*    --- PARAMETRAR TILL POSTSUM                                          
000095*                                                                         
000096*01  -COPY W0005   -PRE  POSTSUM-                                         
000097     EJECT                                                                
000098 01  IN-AREA-START               PIC X(24)   VALUE                        
000099                                 'IN-AREA-START  '.                       
000100     SKIP2                                                                
000101                                                                          
000102*01  AREA -COPY W33014     -PRE IN-                                       
000103     EJECT                                                                
000104 01  UT-AREA-START               PIC X(24)   VALUE                        
000105                                 'UT-AREA-START  '.                       
000106     SKIP2                                                                
000107                                                                          
000108*01  AREA -COPY W33026     -PRE UT-                                       
000109     EJECT                                                                
000110 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
000111                                  'SORTWS-AREA-START  '.                  
000112     SKIP2                                                                
000113                                                                          
000114*01  AREA -COPY W33014      -PRE SORTWS-                                  
000115 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
000116     EJECT                                                                
000117*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000118*                                                                         
000119     EJECT                                                                
000120 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000121     SKIP3                                                                
000122 01  NYCKLAR-TILL-DLI.                                                    
000123     03  W-WDB101KY-X.                                                    
000124         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
000125         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
000126                                                                          
000127     03  W-IDPROMR-X.                                                     
000128         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
000129*   NYCKLAR TILL KUNDREG             ***********                          
000130     03  W-IDGMT-MIN-X.                                                   
000131         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
000132         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
000133                                                                          
000134     03  W-IDGMT-MAX-X.                                                   
000135         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
000136         05  W-IDKUNDNR-B2       PIC S9(7)                                
000137                                 VALUE +9999999 COMP-3.                   
000138                                                                          
000139     SKIP2                                                                
000140*    --- STATUS-KOD FRÅN IMS                                              
000141 01  STATUS-WS                   PIC XX.                                  
000142     88  SEGMENT-FINNS                       VALUE '  '.                  
000143     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000144     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000145     SKIP2                                                                
000146 01  GODK-STATUSKODER.                                                    
000147     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000148     SKIP3                                                                
000149 01  SSA1                        PIC X(64).                               
000150 01  SSA2                        PIC X(64).                               
000151     EJECT                                                                
000152*    --- IMS FUNKTIONSKODER                                               
000153*01  -COPY W0003                                                          
000154     EJECT                                                                
000155*    ---  DLI INPUT-OUTPUT AREA                                           
000156 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000157     SKIP3                                                                
000158 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
000159 01  DLI-IO-B201.                                                         
000160*    03  -COPY WDB201     -PRE GMTA-                                      
000167     SKIP3                                                                
000168                                                                          
000169 01  FILLER               PIC X(16)   VALUE 'WDB101 AREA'.                
000170 01  DLI-IO-B101.                                                         
000171*    03  -COPY WDB101     -PRE WDB101-                                    
000172     SKIP3                                                                
000174     EJECT                                                                
000175 LINKAGE SECTION.                                                         
000176*01  -COPY W0008  -PRE GMTA-                                              
000177     05  FILLER                  PIC X.                                   
000178                                                                          
000179*01  -COPY W0008  -PRE WDB1-                                              
000180     05  FILLER                  PIC X.                                   
000181     EJECT                                                                
000182 PROCEDURE DIVISION  USING GMTA-PCB WDB1-PCB.                             
000183 MAIN SECTION.                                                            
000184     ENTRY 'DLITCBL' USING GMTA-PCB WDB1-PCB.                             
000185                                                                          
000186     PERFORM A-INIT                                                       
000187                                                                          
000188     SORT SORTFIL ASCENDING KEY SORT-IDDISTR                              
000189                                                                          
000190                  USING W33015                                            
000191                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
000192                                                                          
000193     IF SORT-RETURN NOT = 0                                               
000194       MOVE SORT-RETURN TO SORT-RETURN-X                                  
000195       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
000196           DELIMITED BY SIZE                                              
000197           INTO FELTEXT-STR                                               
000198       DISPLAY FELTEXT                                                    
000199       PERFORM S99-ABEND                                                  
000200     ELSE                                                                 
000201       PERFORM Z-FINIT                                                    
000202                                                                          
000203       MOVE ZERO TO RETURN-CODE                                           
000204       GOBACK                                                             
000205     END-IF                                                               
000206                                                                          
000207     .                                                                    
000208     EJECT                                                                
000209 A-INIT SECTION.                                                          
000210                                                                          
000211     OPEN OUTPUT W33026                                                   
000212                                                                          
000213     ACCEPT DAGENS-DATUM  FROM DATE                                       
000214     MOVE   IDPGM         TO   POSTSUM-PROGNAMN                           
000215     .                                                                    
000216     EJECT                                                                
000217 B-SORT-OUTPUT SECTION.                                                   
000218                                                                          
000219     PERFORM S31-SORT-RETURN                                              
000220     PERFORM UNTIL END-OF-SORTFIL                                         
000221       MOVE SORTWS-AREA TO UT-AREA                                        
000222       IF SORT-IDDISTR NOT = W-IDDISTR-B2                                 
000223         PERFORM BA-HAEMTA-PRISOMR                                        
000224       END-IF                                                             
000225       PERFORM BB-FLYTTA-O-SKRIV                                          
000226       PERFORM S31-SORT-RETURN                                            
000227     END-PERFORM                                                          
000228     .                                                                    
000229     EJECT                                                                
000230 BA-HAEMTA-PRISOMR SECTION.                                               
000231                                                                          
000232     MOVE SORT-IDDISTR TO W-IDDISTR-B2                                    
000233                          W-IDDISTR-B1                                    
000234                                                                          
000235                                                                          
000236     PERFORM IMS-GET-WLGMTA01                                             
000237     IF SEGMENT-FINNS                                                     
000238        MOVE GMTA-GMT-IDPARTNR            TO W-WDB1-IDPARTNR              
000239        MOVE GMTA-GMT-IDFTG               TO W-WDB1-IDFTG                 
000240        PERFORM IMS-GET-WDB101                                            
000241        IF SEGMENT-FINNS                                                  
000242           MOVE WDB101-BET-IDMARKBO       TO SPAR-IDMARKBO                
000243           MOVE WDB101-BET-IDPROMRN       TO SPAR-IDPROMRN                
000244        ELSE                                                              
000245           DISPLAY 'DISTRIKTET SAKNAS PÅ WDB1'                            
000246                   SORT-IDDISTR                                           
000247           MOVE SPACE                     TO SPAR-IDMARKBO                
000248                                             SPAR-IDPROMRN                
000249*          CALL ABEND                                                     
000250        END-IF                                                            
000251     ELSE                                                                 
000252       DISPLAY 'DISTRIKTET SAKNAS PÅ GMTA'                                
000253               SORT-IDDISTR                                               
000254       MOVE SPACE                     TO SPAR-IDMARKBO                    
000255                                         SPAR-IDPROMRN                    
000256*      CALL ABEND                                                         
000257     END-IF                                                               
000258     .                                                                    
000259     EJECT                                                                
000260 BB-FLYTTA-O-SKRIV SECTION.                                               
000261                                                                          
000262                                                                          
000263     MOVE SORT-IDARTNR             TO UT-IDARTNR                          
000264     MOVE SPAR-IDMARKBO            TO UT-IDMARKBO                         
000265     MOVE SPAR-IDPROMRN            TO UT-IDPROMRN                         
000266     MOVE SORT-IDDISTR             TO UT-IDDISTR                          
000267     MOVE SORT-IDKONCNR            TO UT-IDKONCNR                         
000268     MOVE SORT-KDMARK-BUDG         TO UT-KDMARK-BUDG                      
000269     MOVE SORT-BEMARK-BUDG         TO UT-BEMARK-BUDG                      
000270     MOVE SORT-SUARTFSG-PER        TO UT-SUARTFSG-PER                     
000271     MOVE SORT-SUARTFSG-AAR        TO UT-SUARTFSG-AAR                     
000272     MOVE SORT-SUARTFSG-FAAR       TO UT-SUARTFSG-FAAR                    
000273     MOVE SORT-SUARTFSG-RAAR       TO UT-SUARTFSG-RAAR                    
000274     MOVE SORT-SUARTFSG-FRAAR      TO UT-SUARTFSG-FRAAR                   
000275     MOVE SORT-SULEVANT-PER        TO UT-SULEVANT-PER                     
000276     MOVE SORT-SULEVANT-AAR        TO UT-SULEVANT-AAR                     
000277     MOVE SORT-SULEVANT-FAAR       TO UT-SULEVANT-FAAR                    
000278     MOVE SORT-SULEVANT-RAAR       TO UT-SULEVANT-RAAR                    
000279     MOVE SORT-SULEVANT-FRAAR      TO UT-SULEVANT-FRAAR                   
000280     MOVE SORT-SUARTSJK-PER        TO UT-SUARTSJK-PER                     
000281     MOVE SORT-SUARTSJK-AAR        TO UT-SUARTSJK-AAR                     
000282     MOVE SORT-SUARTSJK-FAAR       TO UT-SUARTSJK-FAAR                    
000283     MOVE SORT-SUARTSJK-RAAR       TO UT-SUARTSJK-RAAR                    
000284     MOVE SORT-SUARTSJK-FRAAR      TO UT-SUARTSJK-FRAAR                   
000285     MOVE SORT-SULEVANT-RAAR-SPEC  TO UT-SULEVANT-RAAR-SPEC               
000286     MOVE SORT-SULEVANT-RAAR-RAB   TO UT-SULEVANT-RAAR-RAB                
000287     MOVE SORT-SULEVANT-RAAR-MAN   TO UT-SULEVANT-RAAR-MAN                
000288     MOVE SORT-SULEVANT-RAAR-KRE   TO UT-SULEVANT-RAAR-KRE                
000289     MOVE SORT-SUARTFSG-RAAR-SPEC  TO UT-SUARTFSG-RAAR-SPEC               
000290     MOVE SORT-SUARTFSG-RAAR-RAB   TO UT-SUARTFSG-RAAR-RAB                
000291     MOVE SORT-SUARTFSG-RAAR-MAN   TO UT-SUARTFSG-RAAR-MAN                
000292     MOVE SORT-SUARTFSG-RAAR-KRE   TO UT-SUARTFSG-RAAR-KRE                
000293     MOVE SORT-SUARTFSG-DO-RAAR    TO UT-SUARTFSG-DO-RAAR                 
000294     MOVE SORT-SULEVANT-DO-RAAR    TO UT-SULEVANT-DO-RAAR                 
000295                                                                          
000296                                                                          
000297     PERFORM S11-SKRIV-W33026                                             
000298     .                                                                    
000299     EJECT                                                                
000300 Z-FINIT SECTION.                                                         
000301     CLOSE W33026                                                         
000302     SKIP2                                                                
000303     MOVE 'S' TO POSTSUM-OPKOD                                            
000304     CALL POSTSUM USING POSTSUM-PARM                                      
000305     .                                                                    
000306     EJECT                                                                
000307 S11-SKRIV-W33026 SECTION.                                                
000308                                                                          
000309     WRITE UT-POST FROM UT-AREA                                           
000310                                                                          
000311     MOVE 'UT' TO POSTSUM-TRANSTYP                                        
000312     MOVE 'W33026' TO POSTSUM-FDNAMN                                      
000313     MOVE 'W33022D2' TO POSTSUM-DDNAMN2                                   
000314     CALL POSTSUM USING POSTSUM-PARM                                      
000315     .                                                                    
000316     EJECT                                                                
000317 S31-SORT-RETURN  SECTION.                                                
000318                                                                          
000319     RETURN SORTFIL INTO SORTWS-AREA                                      
000320     AT END                                                               
000321         SET END-OF-SORTFIL TO TRUE                                       
000322     .                                                                    
000323     EJECT                                                                
000324 S99-ABEND SECTION.                                                       
000325                                                                          
000326     SKIP2                                                                
000327     MOVE 'S' TO POSTSUM-OPKOD                                            
000328     CALL POSTSUM USING POSTSUM-PARM                                      
000329     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000330     .                                                                    
000331     EJECT                                                                
000332* --- IMS SEKTIONER ---                                                   
000333     SKIP3                                                                
000334 IMS-GET-WLGMTA01  SECTION.                                               
000335                                                                          
000336     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
000337                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
000338             DELIMITED BY SIZE INTO SSA1                                  
000339     MOVE '  GE' TO GODK-STATUSKODER                                      
000340     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-B201 SSA1                      
000341     MOVE GMTA-STATUS-CODE  TO STATUS-WS                                  
000342     PERFORM IMS-STATUSKONTROLL                                           
000343     .                                                                    
000344     SKIP2                                                                
000345 IMS-GET-WDB101 SECTION.                                                  
000346                                                                          
000347     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
000348          DELIMITED BY SIZE INTO SSA1                                     
000349     MOVE '  GE' TO GODK-STATUSKODER                                      
000350     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-B101 SSA1                      
000351     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000352     PERFORM IMS-STATUSKONTROLL                                           
000353     .                                                                    
000354     EJECT                                                                
000355 IMS-STATUSKONTROLL SECTION.                                              
000356                                                                          
000357     SET STATUS-IX TO 1                                                   
000358     SEARCH GODK-STATUS                                                   
000359       AT END                                                             
000360         MOVE 'IMS ABEND' TO FELTEXT-STR                                  
000361         DISPLAY FELTEXT                                                  
000362         DISPLAY 'STATUS ' STATUS-WS '*'                                  
000363                                                                          
000364         CALL FELLOG                                                      
000365       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000366         CONTINUE                                                         
000367     END-SEARCH                                                           
000370     .                                                                    
