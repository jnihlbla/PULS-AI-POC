000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6121800.                                                
000003 AUTHOR.         JOHAN LINDKVIST.                                         
000004 DATE-WRITTEN.   98/02/25.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PROGRAMMET SKRIVER EN UTFIL MED LAGERPLATSINFO IFRÅN             
000009*        WDJ8 OCH FREKVENSKOD SAMT LAGRINGSKOD                            
000010*                                                                         
000011*        PROGRAMMET LÄSER      WLLOCA (WDJ8)                              
000012*                              WL6313 WL6314 WL6315 WL6316 (WDR2)         
000013*                                                                         
000014*    ABENDKODER:                                                          
000015*        U0016 -  . . . .                                                 
000016*        U1000 -  . . . .                                                 
000017*                                                                         
000018                                                                          
000019     SKIP3                                                                
000020 ENVIRONMENT DIVISION.                                                    
000021     SKIP2                                                                
000022 INPUT-OUTPUT SECTION.                                                    
000023                                                                          
000024 FILE-CONTROL.                                                            
000025     SKIP2                                                                
000026*          --- UTFIL .........                                            
000027     SELECT W61218                     ASSIGN TO W61218D1.                
000028     EJECT                                                                
000029 DATA DIVISION.                                                           
000030     SKIP2                                                                
000031 FILE SECTION.                                                            
000032     SKIP3                                                                
000033 FD  W61218                                                               
000034     RECORDING       F                                                    
000035     BLOCK CONTAINS  0.                                                   
000036                                                                          
000037*01  POST -COPY W61218 -PRE  UT1-  -L.                                    
000038     EJECT                                                                
000039 WORKING-STORAGE SECTION.                                                 
000040                                                                          
000041                                                                          
000042*    -- CHECKED BY WY2000                                                 
000043 77  IDPGM                       PIC X(8)    VALUE 'W6121800'.            
000044 77  JA                          PIC X       VALUE 'J'.                   
000045 77  NEJ                         PIC X       VALUE 'N'.                   
000046     EJECT                                                                
000047 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000048 01  FILLER REDEFINES DAGENS-DATUM.                                       
000049     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000050     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000051     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000052     EJECT                                                                
000053 01  DYNAMISKA-SUBPROGRAM.                                                
000054*                                                                         
000055     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000056     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000058     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000059     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000060     SKIP2                                                                
000061                                                                          
000062*    --- MINNESTABELL FÖR DATA IFRÅN WDR2                                 
000063 01  FILLER                      PIC X(16)   VALUE 'TAB1 TABELL'.         
000064 01  FREKVENSTABELL.                                                      
000065    03 IDDC-TAB1 OCCURS 100.                                              
000066       05 FREKV-IDDC        PIC X(2)         VALUE SPACE.                 
000067       05 FREKV-TAB1 OCCURS 30.                                           
000068          07  TAB1-KDFREQ   PIC X(2)         VALUE SPACE.                 
000069          07  TAB1-KVPB-FOM PIC S9(6)V9(1) COMP-3                         
000070                                             VALUE ZERO.                  
000071          07  TAB1-KVPB-TOM PIC S9(6)V9(1) COMP-3                         
000072                                             VALUE ZERO.                  
000073          07  TAB1-TEFREQ   PIC X(10)        VALUE SPACE.                 
000074                                                                          
000075*    --- MINNESTABELL TVÅ FÖR DATA IFRÅN WDR2                             
000076 01  FILLER                      PIC X(16)   VALUE 'STOR TABELL'.         
000077 01  STORAGETABELL.                                                       
000078    03 IDDC-TAB2 OCCURS 100.                                              
000079       05 STOR-IDDC          PIC X(2)       VALUE SPACE.                  
000080       05 STOR-TAB2 OCCURS 150.                                           
000081          07  TAB2-KDSTOR    PIC X(3)       VALUE SPACE.                  
000082          07  TAB2-TESTORAGE PIC X(18)      VALUE SPACE.                  
000083                                                                          
000084*    --- DC/ FREKVENSINDEX                                                
000085 01  DC-IX                       PIC S9(9)   VALUE ZERO.                  
000087 01  DC-IX-MAX                   PIC S9(9)   VALUE 100.                   
000088 01  DCMAX-IX                    PIC S9(9)   VALUE 100.                   
000090 01  FREKV-IX                    PIC S9(9)   VALUE ZERO.                  
000091 01  FREKV-IX-MAX                PIC S9(9)   VALUE 30.                    
000092 01  STOR-IX                     PIC S9(9)   VALUE ZERO.                  
000093 01  STOR-IX-MAX                 PIC S9(9)   VALUE 300.                   
000094 01  SW-IDDC                     PIC X       VALUE SPACE.                 
000095                                                                          
000096*    --- PARAMETRAR TILL ABEND                                            
000097                                                                          
000098 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000099 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000101     SKIP2                                                                
000102 01  FELTEXT.                                                             
000103     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000104     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000105     EJECT                                                                
000106*    --- PARAMETRAR TILL POSTSUM                                          
000107*                                                                         
000108*01  -COPY W0005   -PRE  POSTSUM-                                         
000109     EJECT                                                                
000110 01  UT1-AREA-START              PIC X(24)   VALUE                        
000111                                 'UT1-AREA-START  '.                      
000112     SKIP2                                                                
000113                                                                          
000114*01  AREA -COPY W61218     -PRE UT1-                                      
000115     EJECT                                                                
000116*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000117*                                                                         
000118     EJECT                                                                
000119 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000120                                                                          
000121 01  NYCKLAR-TILL-DLI.                                                    
000122     03  W-WDGXKEY-6313-X.                                                
000123         05  WS1-IDHTYP            PIC X(4)    VALUE '6313'.              
000124         05  WS1-IDDC              PIC X(2)    VALUE SPACE.               
000125         05  WS1-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.           
000126                                                                          
000127     03  W-WDGXKEY-6314-X.                                                
000128         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
000129                                                                          
000130     03  W-WDGXKEY-6315-X.                                                
000131         05  WS2-IDHTYP            PIC X(4)    VALUE '6315'.              
000132         05  WS2-IDDC              PIC X(2)    VALUE SPACE.               
000133         05  WS2-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.           
000134                                                                          
000135     03  W-WDGXKEY-6316-X.                                                
000136         05  W-KDSTOR            PIC X(2)    VALUE SPACE.                 
000137                                                                          
000138     03  W-WDJ801KY-X.                                                    
000139         05  W-WDJ801KY          PIC X(11)   VALUE SPACE.                 
000140     SKIP2                                                                
000141                                                                          
000142*    --- STATUS-KOD FRÅN IMS                                              
000143 01  STATUS-WS                   PIC XX.                                  
000144     88  SEGMENT-FINNS                       VALUE '  '.                  
000145     88  SEGMENT-SAKNAS                      VALUE 'GB'                   
000146                                                   'GE'.                  
000147     88  BASEN-SLUT                          VALUE 'GB'.                  
000148     SKIP2                                                                
000149 01  GODK-STATUSKODER.                                                    
000150     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000151     SKIP3                                                                
000152 01  SSA1                        PIC X(64).                               
000153 01  SSA2                        PIC X(64).                               
000154     EJECT                                                                
000155*    --- IMS FUNKTIONSKODER                                               
000156*01  -COPY W0003                                                          
000157     EJECT                                                                
000158*    ---  DLI INPUT-OUTPUT AREA                                           
000159                                                                          
000160 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
000161 01  WL631301-AREA.                                                       
000162*    03  -COPY WDGX6313                                                   
000163 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
000164 01  WL631311-AREA.                                                       
000165*    03  -COPY WDGX6314                                                   
000166 01  FILLER         PIC X(30) VALUE 'WL631501-AREA'.                      
000167 01  WL631501-AREA.                                                       
000168*    03  -COPY WDGX6315                                                   
000169 01  FILLER         PIC X(23) VALUE 'WL631511-AREA'.                      
000170 01  WL631511-AREA.                                                       
000171*    03  -COPY WDGX6316                                                   
000172 01  FILLER         PIC X(34) VALUE 'DLI-IO-WLLOCA'.                      
000173 01  DLI-IO-WLLOCA.                                                       
000174*    03  -COPY WDJ801                                                     
000175                                                                          
000176 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000177 01   DLI-IO-AREA-B601.                                                   
000178*     03  -COPY WDB601                                                    
000179     EJECT                                                                
000180                                                                          
000181 LINKAGE SECTION.                                                         
000182                                                                          
000183*01  -COPY W0008  -PRE LOCA-                                              
000184     05  FILLER                  PIC X.                                   
000185     EJECT                                                                
000186*01  -COPY W0008  -PRE 6313-                                              
000187     05  FILLER                  PIC X.                                   
000188     EJECT                                                                
000189*01  -COPY W0008  -PRE 6315-                                              
000190     05  FILLER                  PIC X.                                   
000191     EJECT                                                                
000192*01  -COPY W0008  -PRE WDB6-                                              
000193     05  FILLER                  PIC X.                                   
000194     EJECT                                                                
000195                                                                          
000196 PROCEDURE DIVISION  USING LOCA-PCB 6313-PCB 6315-PCB WDB6-PCB.           
000197 MAIN SECTION.                                                            
000198     ENTRY 'DLITCBL' USING LOCA-PCB 6313-PCB 6315-PCB WDB6-PCB.           
000199                                                                          
000200     PERFORM A-INIT                                                       
000201                                                                          
000202     PERFORM B-LAES-FLYTTA-FREKV-STOR-TAB                                 
000203     MOVE DC-IX TO DCMAX-IX                                               
000205     PERFORM IMS-GN-LOCA                                                  
000206     PERFORM UNTIL SEGMENT-SAKNAS                                         
000207       EVALUATE LOCA-SEG-NAME-FB                                          
000208         WHEN 'WDJ801'                                                    
000209           PERFORM C1-HAEMTA-FREKVENSTABELL-INFO                          
000210           IF SW-IDDC = 'J'                                               
000211           PERFORM C2-HAEMTA-STORAGETABELL-INFO                           
000212           PERFORM D-FLYTTA-TILL-UT1-AREA                                 
000213           PERFORM S11-SKRIV-W61218                                       
000214           END-IF                                                         
000215       END-EVALUATE                                                       
000216       PERFORM IMS-GN-LOCA                                                
000217     END-PERFORM                                                          
000218     PERFORM Z-FINIT                                                      
000219                                                                          
000220     MOVE ZERO TO RETURN-CODE                                             
000221     GOBACK                                                               
000222     .                                                                    
000223     EJECT                                                                
000224                                                                          
000225 A-INIT SECTION.                                                          
000226                                                                          
000227     OPEN OUTPUT W61218                                                   
000228                                                                          
000229     ACCEPT DAGENS-DATUM  FROM DATE                                       
000230     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000231                                                                          
000232     INITIALIZE FREKVENSTABELL                                            
000233     INITIALIZE STORAGETABELL                                             
000234     .                                                                    
000235     EJECT                                                                
000236                                                                          
000237 B-LAES-FLYTTA-FREKV-STOR-TAB SECTION.                                    
000238                                                                          
000239     MOVE +1 TO DC-IX                                                     
000240     MOVE +1 TO FREKV-IX                                                  
000241     MOVE +1 TO STOR-IX                                                   
000242                                                                          
000243     PERFORM IMS-GN-WDB601                                                
000244     PERFORM UNTIL BASEN-SLUT OR DC-IX > DC-IX-MAX                        
000245       IF NOT DCS-DDC                                                     
000246        IF DCS-FLWEBDC = 'N'                                              
000247         MOVE DCS-IDDC TO FREKV-IDDC(DC-IX)                               
000248                          WS1-IDDC                                        
000249         PERFORM IMS-GU-WL631301                                          
000250         IF SEGMENT-FINNS                                                 
000251           PERFORM IMS-GNP-WL631311                                       
000252           PERFORM UNTIL SEGMENT-SAKNAS OR FREKV-IX > FREKV-IX-MAX        
000253           IF FREKV-IX > FREKV-IX-MAX                                     
000254            DISPLAY 'TOO MANY WDGX6314 SEGMENTS FOR DC ' DCS-IDDC         
000255              CALL ABEND USING RKOD-ABEND-UTAN-DUMP                       
000256              END-IF                                                      
000257             MOVE 6314-KDFREQ   TO TAB1-KDFREQ   (DC-IX, FREKV-IX)        
000258             MOVE 6314-KVPB-FOM TO TAB1-KVPB-FOM (DC-IX, FREKV-IX)        
000259             MOVE 6314-KVPB-TOM TO TAB1-KVPB-TOM (DC-IX, FREKV-IX)        
000260             MOVE 6314-TEFREQ   TO TAB1-TEFREQ   (DC-IX, FREKV-IX)        
000261             ADD +1 TO FREKV-IX                                           
000262             PERFORM IMS-GNP-WL631311                                     
000263           END-PERFORM                                                    
000264           MOVE +1 TO FREKV-IX                                            
000265         END-IF                                                           
000266                                                                          
000267         MOVE DCS-IDDC TO STOR-IDDC(DC-IX)                                
000268                          WS2-IDDC                                        
000269         PERFORM IMS-GU-WL631501                                          
000270         IF SEGMENT-FINNS                                                 
000271           PERFORM IMS-GNP-WL631511                                       
000272           PERFORM UNTIL SEGMENT-SAKNAS                                   
000273           IF STOR-IX > STOR-IX-MAX                                       
000274            DISPLAY 'TOO MANY WDGX6316 SEGMENTS FOR DC ' DCS-IDDC         
000275              CALL ABEND USING RKOD-ABEND-UTAN-DUMP                       
000276              END-IF                                                      
000277             MOVE 6316-KDSTOR    TO TAB2-KDSTOR   (DC-IX, STOR-IX)        
000278             MOVE 6316-TESTORAGE TO TAB2-TESTORAGE(DC-IX, STOR-IX)        
000279             ADD +1 TO STOR-IX                                            
000280             PERFORM IMS-GNP-WL631511                                     
000281           END-PERFORM                                                    
000282           MOVE +1 TO STOR-IX                                             
000283         END-IF                                                           
000284         ADD +1 TO DC-IX                                                  
000285        END-IF                                                            
000286       END-IF                                                             
000287       PERFORM IMS-GN-WDB601                                              
000288     END-PERFORM                                                          
000289     .                                                                    
000290     EJECT                                                                
000291                                                                          
000292 C1-HAEMTA-FREKVENSTABELL-INFO SECTION.                                   
000293                                                                          
000294     MOVE 'N' TO SW-IDDC                                                  
000295     MOVE +1 TO DC-IX                                                     
000296     MOVE +1 TO FREKV-IX                                                  
000297     PERFORM UNTIL DC-IX > DCMAX-IX                                       
000298       IF FREKV-IDDC (DC-IX) = LOC-IDDC                                   
000299         PERFORM UNTIL FREKV-IX > FREKV-IX-MAX                            
000300           IF TAB1-KDFREQ (DC-IX, FREKV-IX) = LOC-KDFREQ                  
000301             MOVE TAB1-KVPB-FOM (DC-IX, FREKV-IX) TO UT1-KVPB-FOM         
000302             MOVE TAB1-KVPB-TOM (DC-IX, FREKV-IX) TO UT1-KVPB-TOM         
000303             MOVE TAB1-TEFREQ   (DC-IX, FREKV-IX) TO UT1-TEFREQ           
000304             MOVE 'J' TO SW-IDDC                                          
000305             MOVE FREKV-IX-MAX TO FREKV-IX                                
000306           END-IF                                                         
000307           ADD +1 TO FREKV-IX                                             
000308         END-PERFORM                                                      
000309         MOVE +1 TO FREKV-IX                                              
000310       END-IF                                                             
000311       ADD +1 TO DC-IX                                                    
000312     END-PERFORM                                                          
000313     .                                                                    
000314     EJECT                                                                
000315                                                                          
000316 C2-HAEMTA-STORAGETABELL-INFO SECTION.                                    
000317                                                                          
000318     MOVE +1 TO DC-IX                                                     
000319     MOVE +1 TO STOR-IX                                                   
000320     PERFORM UNTIL DC-IX > DCMAX-IX                                       
000321       IF STOR-IDDC (DC-IX) = LOC-IDDC                                    
000322         PERFORM UNTIL STOR-IX > STOR-IX-MAX                              
000323           IF TAB2-KDSTOR (DC-IX, STOR-IX) = LOC-KDSTOR                   
000324             MOVE TAB2-TESTORAGE (DC-IX, STOR-IX) TO UT1-TESTORAGE        
000325             MOVE STOR-IX-MAX TO STOR-IX                                  
000326           END-IF                                                         
000327           ADD +1 TO STOR-IX                                              
000328         END-PERFORM                                                      
000329         MOVE +1 TO STOR-IX                                               
000330       END-IF                                                             
000331       ADD +1 TO DC-IX                                                    
000332     END-PERFORM                                                          
000333     .                                                                    
000334     EJECT                                                                
000335                                                                          
000336 D-FLYTTA-TILL-UT1-AREA SECTION.                                          
000337                                                                          
000338     MOVE LOC-IDDC      TO UT1-IDDC                                       
000339     MOVE LOC-ADLAGOMR  TO UT1-ADLAGOMR                                   
000340     MOVE LOC-ADGANG    TO UT1-ADGANG                                     
000341     MOVE LOC-ADPLATS   TO UT1-ADPLATS                                    
000342     MOVE LOC-KDLOC     TO UT1-KDLOC                                      
000343     MOVE LOC-KDFREQ    TO UT1-KDFREQ                                     
000344     MOVE LOC-KDSTOR    TO UT1-KDSTOR                                     
000345     .                                                                    
000346     EJECT                                                                
000347                                                                          
000348 Z-FINIT SECTION.                                                         
000349     CLOSE W61218                                                         
000350     SKIP2                                                                
000351     MOVE 'S' TO POSTSUM-OPKOD                                            
000352     CALL POSTSUM USING POSTSUM-PARM                                      
000353     .                                                                    
000354     EJECT                                                                
000355                                                                          
000356 S11-SKRIV-W61218 SECTION.                                                
000357                                                                          
000358     WRITE UT1-POST FROM UT1-AREA                                         
000359                                                                          
000360     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000361     MOVE 'W61218' TO POSTSUM-FDNAMN                                      
000362     MOVE 'W61218D1' TO POSTSUM-DDNAMN2                                   
000363     CALL POSTSUM USING POSTSUM-PARM                                      
000364     .                                                                    
000365     EJECT                                                                
000366* --- IMS SEKTIONER ---                                                   
000367     SKIP3                                                                
000368     EJECT                                                                
000369                                                                          
000370 IMS-GU-WL631301 SECTION.                                                 
000371                                                                          
000372     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
000373          DELIMITED BY SIZE INTO SSA1                                     
000374     MOVE '  GE' TO GODK-STATUSKODER                                      
000375     CALL CBLTDLI USING GU 6313-PCB WL631301-AREA SSA1                    
000376     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
000377     PERFORM IMS-STATUSKONTROLL                                           
000378     .                                                                    
000379     SKIP3                                                                
000380                                                                          
000381 IMS-GNP-WL631311 SECTION.                                                
000382                                                                          
000383     MOVE 'WL631311 ' TO SSA1                                             
000384     MOVE '  GE' TO GODK-STATUSKODER                                      
000385     CALL CBLTDLI USING GNP 6313-PCB WL631311-AREA SSA1                   
000386     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
000387     PERFORM IMS-STATUSKONTROLL                                           
000388     .                                                                    
000389     SKIP3                                                                
000390                                                                          
000391 IMS-GU-WL631501 SECTION.                                                 
000392                                                                          
000393     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
000394          DELIMITED BY SIZE INTO SSA1                                     
000395     MOVE '  GE' TO GODK-STATUSKODER                                      
000396     CALL CBLTDLI USING GU 6315-PCB WL631501-AREA SSA1                    
000397     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
000398     PERFORM IMS-STATUSKONTROLL                                           
000399     .                                                                    
000400     SKIP3                                                                
000401                                                                          
000402 IMS-GNP-WL631511 SECTION.                                                
000403                                                                          
000404     MOVE 'WL631511 ' TO SSA1                                             
000405     MOVE '  GE' TO GODK-STATUSKODER                                      
000406     CALL CBLTDLI USING GNP 6315-PCB WL631511-AREA SSA1                   
000407     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
000408     PERFORM IMS-STATUSKONTROLL                                           
000409     .                                                                    
000410     SKIP3                                                                
000411 IMS-GN-LOCA   SECTION.                                                   
000412                                                                          
000413     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA                         
000414     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
000415     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000416     PERFORM IMS-STATUSKONTROLL                                           
000417     .                                                                    
000418     EJECT                                                                
000419 IMS-GN-WDB601    SECTION.                                                
000420     MOVE 'WDB601  ' TO SSA1                                              
000421     MOVE '  GB' TO GODK-STATUSKODER                                      
000422     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000423     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000424     PERFORM IMS-STATUSKONTROLL                                           
000425     .                                                                    
000426                                                                          
000427 IMS-STATUSKONTROLL SECTION.                                              
000428                                                                          
000429     SET STATUS-IX TO 1                                                   
000430     SEARCH GODK-STATUS                                                   
000431       AT END                                                             
000432         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000433           DELIMITED BY SIZE INTO FELTEXT                                 
000434         DISPLAY FELTEXT                                                  
000435         CALL FELLOG                                                      
000436       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000437         CONTINUE                                                         
000438     END-SEARCH                                                           
000440     .                                                                    
