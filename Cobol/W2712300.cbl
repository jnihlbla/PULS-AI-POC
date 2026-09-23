000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W2712300.                                                
000004 AUTHOR.         STEFAN ANDREASSON.                                       
000005 DATE-WRITTEN.   99/01/27.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*                                                                         
000010*                                                                         
000011*                                                                         
000012*        PROGRAMMET KOMPLETTERAR LOKALA KÖP MED                           
000013*        LEDTIDEN FRÅN ARTIKELREG ELLER LEVERANTÖRSREG                    
000014*                                                                         
000015*                                                                         
000016*                                                                         
000017*        PROGRAMMET LÄSER      WLART7 (WDK7)                              
000018*                              WLLEVA (WDF1)                              
000019*                                                                         
000020*    ABENDKODER:                                                          
000021*        U0016 -  . . . .                                                 
000022*        U1000 -  . . . .                                                 
000023*                                                                         
000024                                                                          
000025     SKIP3                                                                
000026 ENVIRONMENT DIVISION.                                                    
000027     SKIP2                                                                
000028 INPUT-OUTPUT SECTION.                                                    
000029                                                                          
000030 FILE-CONTROL.                                                            
000031     SKIP2                                                                
000032*          ---                                                            
000033     SELECT W27115                     ASSIGN TO W27123D1.                
000034     SKIP2                                                                
000035*          ---                                                            
000036     SELECT W27123                     ASSIGN TO W27123D2.                
000037     EJECT                                                                
000038 DATA DIVISION.                                                           
000039     SKIP2                                                                
000040 FILE SECTION.                                                            
000041     SKIP3                                                                
000042 FD  W27115                                                               
000043     RECORDING       F                                                    
000044     BLOCK CONTAINS  0.                                                   
000045*01  -COPY W27115      -L.                                                
000046                                                                          
000047                                                                          
000048 FD  W27123                                                               
000049     RECORDING       F                                                    
000050     BLOCK CONTAINS  0.                                                   
000051                                                                          
000052*01  POST -COPY W27115 -PRE UT-    -L.                                    
000053                                                                          
000054     EJECT                                                                
000055 WORKING-STORAGE SECTION.                                                 
000056                                                                          
000057*    -CHECKED BY WY2000                                                   
000058     SKIP3                                                                
000059 77  IDPGM                       PIC X(8)    VALUE 'W2712300'.            
000060 77  JA                          PIC X       VALUE 'J'.                   
000061 77  NEJ                         PIC X       VALUE 'N'.                   
000062                                                                          
000063 77  W27115-EOF-SW               PIC X       VALUE 'N'.                   
000064     88  END-OF-W27115                       VALUE 'J'.                   
000065                                                                          
000066*01  -COPY WWDCKONS                                                       
000067                                                                          
000068     EJECT                                                                
000069 01  ARBETSAREOR.                                                         
000070                                                                          
000071     03 FILLER                   PIC X(8)   VALUE 'IMS-CALL'.             
000072     03 WS-IMS-CALL              PIC X(16)  VALUE SPACE.                  
000073     03 WS-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.            
000074     03 WS-TIME                  PIC  9(8)  VALUE ZERO.                   
000075     03 WS-PAGE                  PIC  9(4)  VALUE ZERO.                   
000076     03 WS-RADANT                PIC  9(2)  VALUE ZERO.                   
000077     03 WS-SPARA-IDLEVNR         PIC X(5)   VALUE SPACE.                  
000078     03 WS-SPARA-IDDC            PIC X(2)   VALUE SPACE.                  
000079     03 WS-RED-DATUM.                                                     
000080       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
000081       05  FILLER                PIC X      VALUE '/'.                    
000082       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
000083       05  FILLER                PIC X      VALUE '/'.                    
000084       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
000085     03 WS-TIPRLIST              PIC 9(8)   VALUE ZERO.                   
000086     03 WS-DAGENS-DATUM          PIC 9(8)   VALUE ZERO.                   
000087     03 WS-HELTAL                PIC 9(8)   VALUE ZERO.                   
000088     03 WS-HELTAL-RED            PIC Z(5)9  VALUE ZERO.                   
000089     03 WS-DECIMAL               PIC 9(3)   VALUE ZERO.                   
000090     03 WS-PRARTBEL-PR           PIC 9(8)V9(3)                            
000091                                            VALUE ZERO.                   
000092     03 WS-IDAVTAL               PIC 9(12)  VALUE ZERO.                   
000093     03 WS-RED-IDAVTAL           PIC X(14)  VALUE ZERO.                   
000094     03 WS-IDBENR                PIC S9     VALUE ZERO COMP-3.            
000095     03 WS-DAPRLIST              PIC 9(8)   VALUE ZERO.                   
000096                                                                          
000097     03  TAB-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
000098     03  FIL-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
000099     03  IX                      PIC S9(9)  VALUE ZERO COMP-3.            
000100                                                                          
000101                                                                          
000102 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000103 01  FILLER REDEFINES DAGENS-DATUM.                                       
000104     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000105     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000106     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000107                                                                          
000108 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
000109                                                                          
000110     EJECT                                                                
000111                                                                          
000112******************************************************************        
000113*      TABELLER                                                           
000114******************************************************************        
000115                                                                          
000116                                                                          
000117 01  TABENTRY-PARM.                                                       
000118                                                                          
000119     03  STEGLANGD                 PIC S9(9) COMP.                        
000120     03  POST-ANTAL                PIC S9(9) COMP.                        
000121     03  NYCKELLANGD               PIC S9(9) COMP.                        
000122                                                                          
000123 01  TAB-MAX                     PIC S9(9) COMP  VALUE ZERO.              
000124                                                                          
000125 01  RETURANTALTABELL.                                                    
000126     03  RETURANTAL OCCURS 5.                                             
000127        05  TAB-IDDC                  PIC  X(2).                          
000128        05  TAB-ANTAL                 PIC S9(7)    COMP-3.                
000129                                                                          
000130 01  DYNAMISKA-SUBPROGRAM.                                                
000131*                                                                         
000132     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000133     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000134     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000135     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000136     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
000137     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000138     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
000139     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
000140     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
000141     SKIP2                                                                
000142*    --- PARAMETRAR TILL ABEND                                            
000143                                                                          
000144 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000145 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000146     SKIP2                                                                
000147 01  FELTEXT.                                                             
000148     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000149     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000150     EJECT                                                                
000151*    --- PARAMETRAR TILL DATKORT                                          
000152*                                                                         
000153 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27123'.              
000154     SKIP2                                                                
000155 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000156     SKIP2                                                                
000157*01  -COPY WDATKORT                                                       
000158     EJECT                                                                
000159*******                      PARAMETRAR TILL WORKDAY                      
000160*                                                                         
000161*01  -COPY WORKAREA                                                       
000162     EJECT                                                                
000163*01  -COPY WDATAREA                                                       
000164     EJECT                                                                
000165*******                      PARAMETRAR TILL WDAGKONV                     
000166*                                                                         
000167*01  -COPY WDAGAREA                                                       
000168     EJECT                                                                
000169 01  W27115-AREA-START           PIC X(24)   VALUE                        
000170                                 'W27115-AREA-START  '.                   
000171     SKIP2                                                                
000172                                                                          
000173*01  AREA -COPY W27115     -PRE W27115-                                   
000174     EJECT                                                                
000175     EJECT                                                                
000176*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000177*                                                                         
000178     EJECT                                                                
000179 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000180     SKIP3                                                                
000181 01  NYCKLAR-TILL-DLI.                                                    
000182     03  W-IDARTNR-X.                                                     
000183         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000184     03  W-IDDC-X.                                                        
000185         05  W-IDDC              PIC X(02)    VALUE SPACE.                
000186     03  W-IDLEVNR-X.                                                     
000187         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000188     03  W-KDSEGKEY-X.                                                    
000189         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
000190     03  W-IDSKYLT-X.                                                     
000191        05 W-IDSKYLT             PIC X(3)   VALUE 'GB '.                  
000192     03  W-WDGXKEY-X.                                                     
000193          05 W-IDHTYP            PIC X(4)    VALUE '2503'.                
000194          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
000195     03  W-IDDC-B6-X.                                                     
000196         05 W-IDDC-B6                  PIC X(2).                          
000197     SKIP2                                                                
000198*    --- STATUS-KOD FRÅN IMS                                              
000199 01  STATUS-WS                   PIC XX.                                  
000200     88  SEGMENT-FINNS                       VALUE '  '.                  
000201     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000202     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
000203                                                   'GB'.                  
000204     SKIP2                                                                
000205 01  GODK-STATUSKODER.                                                    
000206     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000207     SKIP3                                                                
000208 01  SSA1                        PIC X(64).                               
000209 01  SSA2                        PIC X(64).                               
000210     EJECT                                                                
000211*    --- IMS FUNKTIONSKODER                                               
000212*01  -COPY W0003                                                          
000213     EJECT                                                                
000214******************************************************************        
000215*          DLI INPUT - OUTPUT AREA                                        
000216******************************************************************        
000217                                                                          
000218                                                                          
000219                                                                          
000220     EJECT                                                                
000221 01  FILLER                  PIC X(16) VALUE 'DLI-IO-LEVA01'.             
000222     SKIP3                                                                
000223 01  DLI-IO-AREA-LEVA01.                                                  
000224*    03  -COPY WDF101                                                     
000225*                                                                         
000226*                                                                         
000227 01  FILLER                  PIC X(16) VALUE 'DLI-IO-LEVA16'.             
000228     SKIP3                                                                
000229 01  DLI-IO-AREA-LEVA16.                                                  
000230*    03  -COPY WDF116                                                     
000231     EJECT                                                                
000232 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTS01'.             
000233     SKIP3                                                                
000234 01  DLI-IO-AREA-ARTS01.                                                  
000235*        05  -COPY WDK701                                                 
000236     EJECT                                                                
000237 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTS11'.             
000238     SKIP3                                                                
000239 01  DLI-IO-AREA-ARTS11.                                                  
000240*        05  -COPY WDK711                                                 
000241     SKIP3                                                                
000242 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000243 01   DLI-IO-AREA-B601.                                                   
000244*     03  -COPY WDB601                                                    
000245     EJECT                                                                
000246                                                                          
000247     EJECT                                                                
000248 LINKAGE SECTION.                                                         
000249                                                                          
000252*01  -COPY W0008  -PRE LEVA-                                              
000253     05  FILLER                  PIC X.                                   
000254*01  -COPY W0008  -PRE  ARTS-                                             
000255     05  FILLER                  PIC X.                                   
000256*01  -COPY W0008  -PRE  WDB6-                                             
000257     05  FILLER                  PIC X.                                   
000258     EJECT                                                                
000259 PROCEDURE DIVISION  USING LEVA-PCB ARTS-PCB WDB6-PCB.                    
000260     ENTRY 'DLITCBL' USING LEVA-PCB ARTS-PCB WDB6-PCB.                    
000261                                                                          
000262     PERFORM A-INIT                                                       
000263     PERFORM B-SKAPA-POSTER                                               
000264     PERFORM Z-FINIT                                                      
000265                                                                          
000266     MOVE ZERO TO RETURN-CODE                                             
000267     GOBACK                                                               
000268     .                                                                    
000269     EJECT                                                                
000270                                                                          
000271                                                                          
000272 A-INIT SECTION.                                                          
000273                                                                          
000274     OPEN INPUT  W27115                                                   
000275                                                                          
000276     OPEN OUTPUT W27123                                                   
000277                                                                          
000278     .                                                                    
000279     EJECT                                                                
000280 B-SKAPA-POSTER SECTION.                                                  
000281     PERFORM S01-LAES-W27115                                              
000282                                                                          
000283     PERFORM UNTIL END-OF-W27115                                          
000284       MOVE W27115-IDDC      TO WS-SPARA-IDDC                             
000285                                W-IDDC                                    
000286                                                                          
000287       MOVE W27115-IDLEVNR   TO WS-SPARA-IDLEVNR                          
000288       MOVE W27115-IDARTNR   TO W-IDARTNR                                 
000289       MOVE W27115-IDLEVNR   TO W-IDLEVNR                                 
000290                                                                          
000291       IF W27115-IDDC NOT = W-IDDC-B6                                     
000292          MOVE W27115-IDDC TO W-IDDC-B6                                   
000293          PERFORM IMS-GU-WDB601                                           
000294       END-IF                                                             
000295                                                                          
000296       PERFORM IMS-GU-WDK7-ARTS11                                         
000297       IF SEGMENT-FINNS                                                   
000298                                                                          
000299         IF SLAG-KVDAGAR-MANLT > ZERO                                     
000300           MOVE SLAG-KVDAGAR-MANLT                                        
000301                             TO W27115-KVDAGAR                            
000302         ELSE                                                             
000303           PERFORM IMS-GET-LEVA-NDC                                       
000304           IF SEGMENT-FINNS                                               
000310              MOVE NDC-KVDAGAR-TBT TO W27115-KVDAGAR                      
000345           END-IF                                                         
000346         END-IF                                                           
000347       END-IF                                                             
000348                                                                          
000349       PERFORM S02-SKRIV-W27123                                           
000350                                                                          
000351       PERFORM S01-LAES-W27115                                            
000352     END-PERFORM                                                          
000353     .                                                                    
000354     EJECT                                                                
000355 Z-FINIT SECTION.                                                         
000356     CLOSE W27115                                                         
000357           W27123                                                         
000358     .                                                                    
000359     EJECT                                                                
000360                                                                          
000361                                                                          
000362 S01-LAES-W27115  SECTION.                                                
000363     READ W27115 INTO W27115-AREA                                         
000364     AT END                                                               
000365        SET END-OF-W27115 TO TRUE                                         
000366                                                                          
000367     END-READ                                                             
000368     .                                                                    
000369     EJECT                                                                
000370                                                                          
000371                                                                          
000372 S02-SKRIV-W27123 SECTION.                                                
000373                                                                          
000374     WRITE UT-POST   FROM W27115-AREA                                     
000375     .                                                                    
000376     EJECT                                                                
000377                                                                          
000378                                                                          
000379* --- IMS SEKTIONER --   &&&                                              
000380     SKIP3                                                                
000381                                                                          
000382                                                                          
000383 IMS-GU-WDK7-ARTS11 SECTION.                                              
000384                                                                          
000385     MOVE 'GU-ARTS11'        TO WS-IMS-CALL                               
000386     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
000387          DELIMITED BY SIZE INTO SSA1                                     
000388     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
000389          DELIMITED BY SIZE INTO SSA2                                     
000390     MOVE '  ' TO GODK-STATUSKODER                                        
000391     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2          
000392     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
000393     PERFORM IMS-STATUSKONTROLL                                           
000394     .                                                                    
000395     SKIP3                                                                
000396                                                                          
000408 IMS-GET-LEVA-NDC SECTION.                                                
000409                                                                          
000410     MOVE 'LEVA-NDC'         TO WS-IMS-CALL                               
000411     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
000412          DELIMITED BY SIZE INTO SSA1                                     
000413     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
000414          DELIMITED BY SIZE INTO SSA2                                     
000415     MOVE '  GE' TO GODK-STATUSKODER                                      
000416     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-LEVA16 SSA1 SSA2          
000417     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
000418     PERFORM IMS-STATUSKONTROLL                                           
000419     .                                                                    
000420 IMS-GU-WDB601    SECTION.                                                
000421     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000422          DELIMITED BY SIZE INTO SSA1                                     
000423     MOVE '  GE' TO GODK-STATUSKODER                                      
000424     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000425     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000426     PERFORM IMS-STATUSKONTROLL                                           
000427     IF SEGMENT-SAKNAS                                                    
000428         MOVE SPACE TO DCS-KDDC                                           
000429     END-IF                                                               
000430     .                                                                    
000431                                                                          
000432 IMS-STATUSKONTROLL SECTION.                                              
000433                                                                          
000434     SET STATUS-IX TO 1                                                   
000435     SEARCH GODK-STATUS                                                   
000436       AT END                                                             
000437         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
000438         DISPLAY FELTEXT                                                  
000439         CALL FELLOG                                                      
000440       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000441         CONTINUE                                                         
000442     END-SEARCH                                                           
000450     .                                                                    
