000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3711000.                                                
000003 AUTHOR.         BO HAMMARIN.                                             
000004 DATE-WRITTEN.   DEC-1999.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*       -PGM SELEKTERAR AKTUELLA POÄNGSALDON OCH BYGGER/SKRIVER;          
000009*        . ORDER-POSTER TILL EVENTUELL FAKTURERING                        
000010*                                                                         
000011*       -PGM LÄSER WDK6                                                   
000012*                  WDGX3156 (WDR1)                                        
000013*                  WDGX3158 (WDR1)                                        
000014*                                                                         
000015*    ABENDKODER:                                                          
000016*        U0016 -  . . . .                                                 
000017*        U1000 -  . . . .                                                 
000018*                                                                         
000019* 9/7 '13 - ETRACKER 10196973, EJ P&H FÖR ADMINISTRATIVA ART.             
000020                                                                          
000021 ENVIRONMENT DIVISION.                                                    
000022                                                                          
000023 INPUT-OUTPUT SECTION.                                                    
000024                                                                          
000025 FILE-CONTROL.                                                            
000026*          --- AKTUELLA ORDER                                             
000027     SELECT W37111                     ASSIGN TO W37110D1.                
000028     EJECT                                                                
000029                                                                          
000030 DATA DIVISION.                                                           
000031                                                                          
000032 FILE SECTION.                                                            
000033 FD  W37111                                                               
000034     RECORDING       F                                                    
000035     BLOCK CONTAINS  0.                                                   
000036                                                                          
000037*01  ORDER-REC     -COPY W37111      -L.                                  
000038     EJECT                                                                
000039                                                                          
000040 WORKING-STORAGE SECTION.                                                 
000041*    -- CHECKED BY WY2000                                                 
000042 77  IDPGM                        PIC X(8)    VALUE 'W3711000'.           
000043 77  JA                           PIC X       VALUE 'J'.                  
000044 77  NEJ                          PIC X       VALUE 'N'.                  
000045 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
000046 77  MAX-INDX                     PIC S9(2)   VALUE +4 COMP SYNC.         
000047 77  SW-FIRST-TIME                PIC X       VALUE 'J'.                  
000048     88  FIRST-TIME                           VALUE 'J'.                  
000049 77  SW-INCLUDED                  PIC X       VALUE 'N'.                  
000050     88  INCLUDED                             VALUE 'J'.                  
000051 77  SW-FAKTURERING               PIC X       VALUE 'N'.                  
000052     88  FAKTURERINGSDAX                      VALUE 'J'.                  
000053                                                                          
000054 01  FELTEXT                      PIC X(80).                              
000055     EJECT                                                                
000056                                                                          
000057 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000058 01  FILLER REDEFINES DAGENS-DATUM.                                       
000059     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000060     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000061     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000062     EJECT                                                                
000063                                                                          
000064 01  DYNAMISKA-SUBPROGRAM.                                                
000065*                                                                         
000066     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000067     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000068     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000069     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000070     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000071     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000072     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
000073     EJECT                                                                
000074                                                                          
000075*    --- PARAMETRAR TILL ABEND                                            
000076                                                                          
000077 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000078 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000079 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000080     EJECT                                                                
000081                                                                          
000082 01  W009VADD-DATUM              PIC S9(5)   VALUE ZERO COMP-3.           
000083 01  W009VADD-ANTAL              PIC S9(3)   VALUE ZERO COMP-3.           
000084     EJECT                                                                
000085                                                                          
000086*    --- PARAMETRAR TILL DATKORT                                          
000087*                                                                         
000088 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37110'.              
000089                                                                          
000090 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000091                                                                          
000092*01  -COPY WDATKORT                                                       
000093     EJECT                                                                
000094                                                                          
000095*    --- PARAMETRAR TILL POSTSUM                                          
000096*                                                                         
000097*01  -COPY W0005   -PRE  POSTSUM-                                         
000098     EJECT                                                                
000099                                                                          
000100*-----------------------------------------PARAMETRAR TILL                 
000101*                                         SUBPROGRAM WDATKONV             
000102 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
000103*01  -COPY WDATAREA.                                                      
000104     EJECT                                                                
000105                                                                          
000106 01  UT-AREA-START               PIC X(24)   VALUE                        
000107                                 'UT-AREA-START  '.                       
000108*01  AREA -COPY W37111     -PRE ORDER-                                    
000109     EJECT                                                                
000110                                                                          
000111*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000112*                                                                         
000113 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000114                                                                          
000115 01  NYCKLAR-TILL-DLI.                                                    
000116     03  W-3155-IDHTYP-X.                                                 
000117         05  W-3155-IDHTYP       PIC  X(4)  VALUE '3155'.                 
000118         05  W-3155-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
000119     03  W-3156-KDSEGKEY-X.                                               
000120         05  W-3156-KDSEGKEY     PIC  X(1)  VALUE '1'.                    
000121                                                                          
000122     03  W-3157-IDHTYP-X.                                                 
000123         05  W-3157-IDHTYP       PIC  X(4)  VALUE '3157'.                 
000124         05  W-3157-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
000125     03  W-3158-WDGXKEY-X.                                                
000126         05  W-3158-IDDISTR      PIC  S9(5) VALUE ZERO COMP-3.            
000127         05  W-3158-KDEXCHA      PIC  S9(3) VALUE ZERO COMP-3.            
000128                                                                          
000129     03  W-IDARTNR-X.                                                     
000130         05  W-IDARTNR           PIC  S9(9) VALUE ZERO COMP-3.            
000131     03  W-KDSEGKEY-X.                                                    
000132         05  W-KDSEGKEY          PIC  X(1)  VALUE '1'.                    
000133     EJECT                                                                
000134                                                                          
000135*    --- STATUS-KOD FRÅN IMS                                              
000136 01  STATUS-WS                   PIC XX.                                  
000137     88  SEGMENT-FINNS                       VALUE '  '.                  
000138     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000139                                                                          
000140 01  GODK-STATUSKODER.                                                    
000141     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000142                                                                          
000143 01  SSA1                        PIC X(64).                               
000144 01  SSA2                        PIC X(64).                               
000145     EJECT                                                                
000146                                                                          
000147*    --- IMS FUNKTIONSKODER                                               
000148*01  -COPY W0003                                                          
000149     EJECT                                                                
000150                                                                          
000151*    ---  DLI INPUT-OUTPUT AREA                                           
000152 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3156'.                    
000153 01  DLI-IO-WDGX3156.                                                     
000154*    03  -COPY WDGX3156                                                   
000155     EJECT                                                                
000156                                                                          
000157 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3157'.                    
000158 01  DLI-IO-WDGX3157.                                                     
000159*    03  -COPY WDGX01                                                     
000160     EJECT                                                                
000161                                                                          
000162 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
000163 01  DLI-IO-WDGX3158.                                                     
000164*    03  -COPY WDGX3158                                                   
000165     EJECT                                                                
000166                                                                          
000167 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611  '.                    
000168 01  DLI-IO-WDK611.                                                       
000169*    03  -COPY WDK611                                                     
000170     EJECT                                                                
000171                                                                          
000172 LINKAGE SECTION.                                                         
000173*01  -COPY W0008  -PRE 3156-                                              
000174     05  FILLER                  PIC X.                                   
000175*01  -COPY W0008  -PRE 3158-                                              
000176     05  FILLER                  PIC X.                                   
000177*01  -COPY W0008  -PRE WDK6-                                              
000178     05  FILLER                  PIC X.                                   
000179     EJECT                                                                
000180                                                                          
000181 PROCEDURE DIVISION  USING 3156-PCB 3158-PCB WDK6-PCB.                    
000182 MAIN SECTION.                                                            
000183     ENTRY 'DLITCBL' USING 3156-PCB 3158-PCB WDK6-PCB.                    
000184                                                                          
000185     PERFORM A-INIT                                                       
000186                                                                          
000187     IF FAKTURERINGSDAX                                                   
000188       PERFORM IMS-GU-WDGX3157                                            
000189       PERFORM IMS-GNP-WDGX3158                                           
000190                                                                          
000191       PERFORM UNTIL SEGMENT-SAKNAS                                       
000192         IF 3158-FLFAKT = 'J' OR 'Y'                                      
000193           PERFORM B-BEARBETA                                             
000194           PERFORM S01-SKRIV-W37111                                       
000195         END-IF                                                           
000196         PERFORM IMS-GNP-WDGX3158                                         
000197       END-PERFORM                                                        
000198     END-IF                                                               
000199                                                                          
000200     PERFORM Z-FINIT                                                      
000201                                                                          
000202     MOVE ZERO TO RETURN-CODE                                             
000203     GOBACK                                                               
000204     .                                                                    
000205     EJECT                                                                
000206                                                                          
000207 A-INIT SECTION.                                                          
000208     PERFORM IMS-GU-WDGX3156                                              
000209                                                                          
000210     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000211     MOVE D-AAR                TO DAGENS-DATUM-AAR                        
000212     MOVE D-MAANAD             TO DAGENS-DATUM-MAANAD                     
000213     MOVE D-DAG                TO DAGENS-DATUM-DAG                        
000214     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
000215                                                                          
000216     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
000217     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
000218                                                                          
000219     CALL WDATKONV USING DAT-KDDATFORM                                    
000220                         DAT-I-TIDATUM                                    
000221                         DAT-O-TIDATUM                                    
000222                         DAT-KDSVAR                                       
000223                                                                          
000224         IF DAT-KDSVAR NOT = ' '                                          
000225           DISPLAY '**** TIAAVV' DAT-I-TIDATUM                            
000226           MOVE +1000 TO RKOD-ABEND-MED-DUMP                              
000227           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
000228         END-IF                                                           
000229                                                                          
000230     MOVE 1 TO INDX                                                       
000231     PERFORM UNTIL INDX > MAX-INDX OR                                     
000232                   FAKTURERINGSDAX                                        
000233       IF DAT-TIVV = 3156-TIVECKNR-BYTDEB (INDX)                          
000234         MOVE JA               TO SW-FAKTURERING                          
000235         MOVE +4               TO INDX                                    
000236       END-IF                                                             
000237       ADD +1                  TO INDX                                    
000238     END-PERFORM                                                          
000239                                                                          
000240     OPEN OUTPUT W37111                                                   
000241     .                                                                    
000242     EJECT                                                                
000243                                                                          
000244 B-BEARBETA SECTION.                                                      
000245     MOVE 3158-KDEXCHA          TO ORDER-KDEXCHA                          
000246                                                                          
000247     IF ORDER-KDEXCHA = 14                                                
000248       MOVE 8609990             TO W-IDARTNR                              
000249     ELSE                                                                 
000250       IF ORDER-KDEXCHA = 24                                              
000251         MOVE 8609993           TO W-IDARTNR                              
000252       ELSE                                                               
000253         IF ORDER-KDEXCHA = 34                                            
000254           MOVE 8609992         TO W-IDARTNR                              
000255         ELSE                                                             
000256           IF ORDER-KDEXCHA = 54                                          
000257             MOVE 8609994       TO W-IDARTNR                              
000258           END-IF                                                         
000259         END-IF                                                           
000260       END-IF                                                             
000261     END-IF                                                               
000262     PERFORM IMS-GU-WDK611                                                
000263*                                                                         
000264*  MAN MÄRKER DE ADMINISTRATIVA ARTIKLARNA MED IDSYST = W37A FÖR          
000265*  ATT KUNNA SKILJA DEM FRÅN DE ANDRA BYTESARTIKLAR EFTERSOM FÖR          
000266*  DESSA ADM. ART. SKALL MAN INTE TA UT P&H SENARE I FAKT.!               
000267*                                                                         
000268     MOVE 'W37A'                TO ORDER-IDSYSTEM                         
000269     MOVE 3158-IDDISTR-BET      TO ORDER-IDDISTR-BET                      
000270     MOVE 3158-IDDISTR          TO ORDER-IDDISTR                          
000271     MOVE ZERO                  TO ORDER-IDKUNDNR                         
000272                                   ORDER-IDORDNR                          
000273     MOVE W-IDARTNR             TO ORDER-IDARTNR                          
000274     MOVE 3158-SUPOINT-BAL      TO ORDER-KVBEART                          
000275     MOVE 3156-REPOINT          TO ORDER-PRARTNTO                         
000276     .                                                                    
000277     EJECT                                                                
000278                                                                          
000279 Z-FINIT SECTION.                                                         
000280     CLOSE W37111                                                         
000281                                                                          
000282     MOVE 'S' TO POSTSUM-OPKOD                                            
000283     CALL POSTSUM USING POSTSUM-PARM                                      
000284     .                                                                    
000285     EJECT                                                                
000286                                                                          
000287 S01-SKRIV-W37111 SECTION.                                                
000288     MOVE SPACE                 TO   ORDER-REC                            
000289     WRITE ORDER-REC            FROM ORDER-AREA                           
000290                                                                          
000291     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
000292     MOVE 'W37111'              TO   POSTSUM-FDNAMN                       
000293     MOVE 'W37110D1'            TO   POSTSUM-DDNAMN2                      
000294     CALL POSTSUM USING POSTSUM-PARM                                      
000295     .                                                                    
000296     EJECT                                                                
000297                                                                          
000298* --- IMS SEKTIONER ---                                                   
000299                                                                          
000300 IMS-GU-WDGX3156 SECTION.                                                 
000301     STRING 'WDR101  (WDGXKEY  =' W-3155-IDHTYP-X ')'                     
000302          DELIMITED BY SIZE INTO SSA1                                     
000303     STRING 'WDGX3156(KDSEGKEY =' W-3156-KDSEGKEY-X ')'                   
000304          DELIMITED BY SIZE INTO SSA2                                     
000305     MOVE '  '              TO   GODK-STATUSKODER                         
000306     CALL CBLTDLI USING GU 3156-PCB DLI-IO-WDGX3156 SSA1 SSA2             
000307     MOVE 3156-STATUS-CODE  TO   STATUS-WS                                
000308     PERFORM IMS-STATUSKONTROLL                                           
000309     .                                                                    
000310     EJECT                                                                
000311                                                                          
000312 IMS-GU-WDGX3157 SECTION.                                                 
000313     STRING 'WDR101  (WDGXKEY  =' W-3157-IDHTYP-X ')'                     
000314          DELIMITED BY SIZE INTO SSA1                                     
000315     MOVE '  '              TO   GODK-STATUSKODER                         
000316     CALL CBLTDLI USING GU 3158-PCB DLI-IO-WDGX3157 SSA1                  
000317     MOVE 3158-STATUS-CODE  TO   STATUS-WS                                
000318     PERFORM IMS-STATUSKONTROLL                                           
000319     .                                                                    
000320                                                                          
000321 IMS-GNP-WDGX3158 SECTION.                                                
000322     MOVE 'WDGX3158  '      TO   SSA1                                     
000323     MOVE '  GE'            TO   GODK-STATUSKODER                         
000324     CALL CBLTDLI USING GNP 3158-PCB DLI-IO-WDGX3158 SSA1                 
000325     MOVE 3158-STATUS-CODE  TO   STATUS-WS                                
000326     PERFORM IMS-STATUSKONTROLL                                           
000327     .                                                                    
000328     EJECT                                                                
000329                                                                          
000330 IMS-GU-WDK611 SECTION.                                                   
000331     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000332          DELIMITED BY SIZE INTO SSA1                                     
000333     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
000334          DELIMITED BY SIZE INTO SSA2                                     
000335     MOVE '  GE'           TO GODK-STATUSKODER                            
000336     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
000337     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000338     PERFORM IMS-STATUSKONTROLL                                           
000339     .                                                                    
000340     EJECT                                                                
000341                                                                          
000342 IMS-STATUSKONTROLL SECTION.                                              
000343     SET STATUS-IX TO 1                                                   
000344     SEARCH GODK-STATUS                                                   
000345       AT END                                                             
000346         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000347           DELIMITED BY SIZE INTO FELTEXT                                 
000348         DISPLAY FELTEXT                                                  
000349         CALL FELLOG                                                      
000350       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000351         CONTINUE                                                         
000352     END-SEARCH                                                           
000353     .                                                                    
