000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W0924200.                                                
000004*AUTHOR.         STEFAN KIHLBERG.                                         
000005*DATE-WRITTEN.   91/09/10.                                                
000006                                                                          
000007*    REMARKS.                                                             
000008*                                                                         
000009*                                                                         
000010*    FUNKTION:                                                            
000011*        LÄSER BASEN WDG901. FÖRDELAR STANSPOSTER TILL                    
000012*        OLIKA FILER MED OLIKA POSTTYPER TILL PROCEDURERNA                
000013*        EFTER TRATTEN, SAMT EN FIL MED KONTROLLPOSTER.                   
000014*                                                                         
000015*        PROGRAMMET UPPDATERAR WLZZAD (WDG9)                              
000016*                                                                         
000017*    ÄNDRINGAR:                                                           
000018*        2001-01-11: IDLEVNR TILL X(5).                                   
000019*                    PÅVERKAR W0924A OCH W0924B                           
000020*                                                                         
000021*    ABENDKODER:                                                          
000022*        U0016 -  . . . .                                                 
000023*        U1000 -  . . . .                                                 
000024*                                                                         
000025                                                                          
000026     EJECT                                                                
000027 ENVIRONMENT DIVISION.                                                    
000028                                                                          
000029 INPUT-OUTPUT SECTION.                                                    
000030                                                                          
000031 FILE-CONTROL.                                                            
000032                                                                          
000033*          --- UTFIL R01-TRANSAR      TILL W213P004                       
000034     SELECT W0924A                     ASSIGN TO W09242D2.                
000035     SKIP2                                                                
000036*          --- UTFIL R02 OCH R17 TRANSAR   TILL W213P002                  
000037     SELECT W0924B                     ASSIGN TO W09242D3.                
000038     SKIP2                                                                
000039*          --- UTFIL R22 OC R23 TRANSAR TILL W212P001                     
000040     SELECT W0924C                     ASSIGN TO W09242D4.                
000041     SKIP2                                                                
000042*          --- UTFIL R05-TRANSAR  TILL W092P021                           
000043     SELECT W0924E                     ASSIGN TO W09242D6.                
000044     EJECT                                                                
000045 DATA DIVISION.                                                           
000046     SKIP2                                                                
000047 FILE SECTION.                                                            
000048     SKIP2                                                                
000049 FD  W0924A                                                               
000050     RECORDING       F                                                    
000051     BLOCK CONTAINS  0.                                                   
000052     SKIP2                                                                
000053*01  POST -COPY W213R01   -PRE W0924A-   -L.                              
000054     SKIP2                                                                
000055 FD  W0924B                                                               
000056     RECORDING       V                                                    
000057     BLOCK CONTAINS  0.                                                   
000058     SKIP2                                                                
000059 01  W0924B-POST        PIC X(32).                                        
000060*01       -COPY W213R02   REDEFINES W0924B-POST    -L.                    
000061     SKIP3                                                                
000062*         -COPY W213R17T  REDEFINES W0924B-POST    -L.                    
000063                                                                          
000064 FD  W0924C                                                               
000065     RECORDING       F                                                    
000066     BLOCK CONTAINS  0.                                                   
000067     SKIP2                                                                
000068 01  W0924C-POST        PIC X(54).                                        
000069*01       -COPY W212R22   REDEFINES W0924C-POST    -L.                    
000070     SKIP3                                                                
000071*         -COPY W212R23   REDEFINES W0924C-POST    -L.                    
000072                                                                          
000073 FD  W0924E                                                               
000074     RECORDING       V                                                    
000075     BLOCK CONTAINS  0.                                                   
000076     SKIP3                                                                
000077 01  W0924E-POST        PIC X(12).                                        
000078                                                                          
000079*01       -COPY W540044   REDEFINES W0924E-POST    -L.                    
000080     EJECT                                                                
000081 WORKING-STORAGE SECTION.                                                 
000082     SKIP2                                                                
000083                                                                          
000084*    -- CHECKED BY WY2000                                                 
000085 77  IDPGM                       PIC X(8)    VALUE 'W0924200'.            
000086 77  JA                          PIC X       VALUE 'J'.                   
000087 77  NEJ                         PIC X       VALUE 'N'.                   
000088                                                                          
000089 77  UPD-RAEKNARE        BINARY  PIC S9(4)   VALUE ZERO.                  
000090                                                                          
000091 77  PG-IX                       PIC S9(1)   VALUE ZERO.                  
000092 77  WS-IDLEVNR-X                PIC X(5)    VALUE SPACE.                 
000093 77  WS-IDARTNR-9                PIC 9(8)    VALUE ZERO.                  
000094                                                                          
000095 01  WS-IDFVARDE.                                                         
000096     03  WS-IDLKTO               PIC 9(7)    VALUE ZERO.                  
000097     03  FILLER                  PIC X(18)   VALUE SPACE.                 
000098     EJECT                                                                
000099                                                                          
000100 01  R01-X.                                                               
000101*    03   -COPY W213R01T  -PRE R01-X- .                                   
000102     SKIP3                                                                
000103     SKIP3                                                                
000104 01  R02-X.                                                               
000105*    03   -COPY W213R02T  -PRE R02-X- .                                   
000106     EJECT                                                                
000107 01  R05-X.                                                               
000108*    03   -COPY W092R05T  -PRE R05-X- .                                   
000109     EJECT                                                                
000110 01  R17-X.                                                               
000111*    03   -COPY W213R17T  -PRE R17-X- .                                   
000112     EJECT                                                                
000113 01  R22-X.                                                               
000114*    03   -COPY W212R22K  -PRE R22-X- .                                   
000115     EJECT                                                                
000116 01  R23-X.                                                               
000117*    03   -COPY W212R22K  -PRE R23-X- .                                   
000118     EJECT                                                                
000119 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000120 01  FILLER REDEFINES DAGENS-DATUM.                                       
000121     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000122     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000123     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000124     SKIP3                                                                
000125 01  DYNAMISKA-SUBPROGRAM.                                                
000126*                                                                         
000127     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000128     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000129     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000130     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000131     SKIP2                                                                
000132*    --- PARAMETRAR TILL ABEND                                            
000133                                                                          
000134 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000135 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000136     SKIP2                                                                
000137 01  FELTEXT.                                                             
000138     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000139     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000140     EJECT                                                                
000141*    --- PARAMETRAR TILL POSTSUM                                          
000142*                                                                         
000143*01  -COPY W0005   -PRE  POSTSUM-                                         
000144     EJECT                                                                
000145                                                                          
000146 01  INTRANS.                                                             
000147     03  IN-MODULNAMN            PIC X(6).                                
000148     03  IN-KORT.                                                         
000149       05  IN-IDKTYP             PIC X(3).                                
000150       05  FILLER                PIC X(157).                              
000151                                                                          
000152*******KONTROLLFIL*******************                                     
000153                                                                          
000154 01  W0924Z-AREA-START           PIC X(24)   VALUE                        
000155                                 'W0924Z-AREA-START  '.                   
000156     SKIP2                                                                
000157 01  W0924Z-AREA.                                                         
000158     03 W-KONTROLL-IDDEL  -COPY W092W001 -PRE KONTROLL-                   
000159                                                                          
000160     03  W-KONTROLL-POST         PIC X(100).                              
000161     03 FILLER  REDEFINES W-KONTROLL-POST.                                
000162        05  FILLER              PIC X(12).                                
000163        05  R05-GLURPKOD        PIC X(16).                                
000164        05  FILLER              PIC X(52).                                
000165     03 FILLER  REDEFINES W-KONTROLL-POST.                                
000166        05  FILLER              PIC X(12).                                
000167        05  R22-IDBESTNR        PIC X(10).                                
000168        05  FILLER              PIC X(57).                                
000169     03 FILLER  REDEFINES W-KONTROLL-POST.                                
000170        05  FILLER              PIC X(20).                                
000171        05  R62-FLANTAL         PIC X(01).                                
000172                                                                          
000173     EJECT                                                                
000174                                                                          
000175***********DATAFILER**********************************                    
000176                                                                          
000177*****   R01  *********                                                    
000178                                                                          
000179                                                                          
000180 01  W0924A-AREA-START           PIC X(24)   VALUE                        
000181                                 'W0924A-AREA-START  '.                   
000182     SKIP2                                                                
000183 01  W0924A-AREA                 PIC X(22).                               
000184*01  AREA -COPY W213R01   -PRE R01- RED W0924A-AREA.                      
000185     EJECT                                                                
000186***** R02 R17  ***************                                            
000187 01  W0924B-AREA-START           PIC X(24)   VALUE                        
000188                                 'W0924B-AREA-START  '.                   
000189     SKIP2                                                                
000190 01  W0924B-AREA                 PIC X(32).                               
000191*01  POST -COPY W213R02  -PRE R02- -RED W0924B-AREA                       
000192     EJECT                                                                
000193*01  POST -COPY W213R17T -PRE R17- -RED W0924B-AREA                       
000194     EJECT                                                                
000195***** R22 R23  ***************                                            
000196 01  W0924C-AREA-START           PIC X(24)   VALUE                        
000197                                 'W0924C-AREA-START  '.                   
000198     SKIP2                                                                
000199 01  W0924C-AREA                 PIC X(54).                               
000200*01  POST -COPY W212R22  -PRE R22- -RED W0924C-AREA                       
000201     EJECT                                                                
000202*01  POST -COPY W212R23  -PRE R23- -RED W0924C-AREA                       
000203     EJECT                                                                
000204***** R05 ********************                                            
000205 01  W0924E-AREA-START           PIC X(24)   VALUE                        
000206                                 'W0924E-AREA-START  '.                   
000207     SKIP2                                                                
000208 01  W0924E-AREA                 PIC X(12).                               
000209*01  POST -COPY W540044  -PRE R05- -RED W0924E-AREA                       
000210     EJECT                                                                
000211*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000212*                                                                         
000213     SKIP3                                                                
000214 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000215     SKIP3                                                                
000216 01  NYCKLAR-TILL-DLI.                                                    
000217     03  W-WDG901KY-X.                                                    
000218         05  W-WDG901KY          PIC S9(09)   VALUE ZERO COMP-3.          
000219     SKIP2                                                                
000220*    --- STATUS-KOD FRÅN IMS                                              
000221 01  STATUS-WS                   PIC XX.                                  
000222     88  SEGMENT-FINNS                       VALUE '  '.                  
000223     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000224     SKIP2                                                                
000225 01  GODK-STATUSKODER.                                                    
000226     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000227     SKIP3                                                                
000228 01  SSA1                        PIC X(64).                               
000229 01  SSA2                        PIC X(64).                               
000230     EJECT                                                                
000231*    --- IMS FUNKTIONSKODER                                               
000232*01  -COPY W0003                                                          
000233     EJECT                                                                
000234*    ---  DLI INPUT-OUTPUT AREA                                           
000235 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000236     SKIP2                                                                
000237 01  DLI-IO-AREA.                                                         
000238     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
000239     SKIP3                                                                
000240     03  WLZZAD01 REDEFINES IO-AREA.                                      
000241*        05  -COPY WDG901  -PRE WLZZAD-                                   
000242     EJECT                                                                
000243     03  WDG901 REDEFINES IO-AREA.                                        
000244         05 FILLER               PIC X(29).                               
000245         05 WDG9-IDPTYP          PIC X(3).                                
000246         05 FILLER               PIC X(87).                               
000247     SKIP3                                                                
000248     03  R01-AREA REDEFINES IO-AREA.                                      
000249         05 FILLER               PIC X(29).                               
000250         05  DATA  -COPY W213R01  -PRE BAS-R01-                           
000251         05  R01-REST             PIC X(75).                              
000252     EJECT                                                                
000253     03  R02-AREA REDEFINES IO-AREA.                                      
000254         05 FILLER               PIC X(29).                               
000255         05  DATA  -COPY W213R02  -PRE BAS-R02-                           
000256         05  R02-REST             PIC X(60).                              
000257     EJECT                                                                
000258     03  R05-AREA REDEFINES IO-AREA.                                      
000259         05 FILLER               PIC X(29).                               
000260         05  DATA  -COPY W092R05T -PRE BAS-R05-                           
000261         05  R05-REST             PIC X(10).                              
000262     EJECT                                                                
000263     03  R17-AREA REDEFINES IO-AREA.                                      
000264         05 FILLER               PIC X(29).                               
000265         05  DATA  -COPY W213R17T -PRE BAS-R17-                           
000266         05  R17-REST             PIC X(67).                              
000267     EJECT                                                                
000268     03  R22-AREA REDEFINES IO-AREA.                                      
000269         05 FILLER               PIC X(29).                               
000270         05  DATA  -COPY W212R22  -PRE BAS-R22-                           
000271         05  R22-REST             PIC X(43).                              
000272     EJECT                                                                
000273     03  R23-AREA REDEFINES IO-AREA.                                      
000274         05 FILLER               PIC X(29).                               
000275         05  DATA  -COPY W212R23  -PRE BAS-R23-                           
000276         05  R23-REST             PIC X(43).                              
000277     EJECT                                                                
000278 LINKAGE SECTION.                                                         
000279                                                                          
000280*01  -COPY W0009  -PRE MSG-                                               
000281     EJECT                                                                
000282*01  -COPY W0008  -PRE ZZAD-                                              
000283     05  FILLER                  PIC X.                                   
000284     EJECT                                                                
000285 PROCEDURE DIVISION  USING MSG-PCB ZZAD-PCB.                              
000286     ENTRY 'DLITCBL' USING MSG-PCB ZZAD-PCB.                              
000287                                                                          
000288     SKIP2                                                                
000289     PERFORM A-INIT                                                       
000290     PERFORM IMS-GHN-WDG901                                               
000291     PERFORM UNTIL SEGMENT-SLUT OR UPD-RAEKNARE > 2000                    
000292        PERFORM S20-NOLLSTALL                                             
000293        EVALUATE WDG9-IDPTYP                                              
000294           WHEN 'R01'                                                     
000295              PERFORM B-SKAPA-R01-W0924A-FIL                              
000296           WHEN 'R02'                                                     
000297              PERFORM C-SKAPA-R02-W0924B-FIL                              
000298           WHEN 'R05'                                                     
000299              PERFORM D-SKAPA-R05-W0924E-FIL                              
000300           WHEN 'R17'                                                     
000301              PERFORM E-SKAPA-R17-W0924B-FIL                              
000302           WHEN 'R22'                                                     
000303              PERFORM F-SKAPA-R22-W0924C-FIL                              
000304           WHEN 'R23'                                                     
000305              PERFORM G-SKAPA-R23-W0924C-FIL                              
000306        END-EVALUATE                                                      
000307        PERFORM IMS-DLET-WDG901                                           
000308        PERFORM IMS-GHN-WDG901                                            
000309                                                                          
000310     END-PERFORM                                                          
000311                                                                          
000312                                                                          
000313     PERFORM Z-FINIT                                                      
000314                                                                          
000315     MOVE ZERO TO RETURN-CODE                                             
000316     GOBACK                                                               
000317     .                                                                    
000318     EJECT                                                                
000319 A-INIT SECTION.                                                          
000320                                                                          
000321     OPEN OUTPUT W0924A                                                   
000322                 W0924B                                                   
000323                 W0924C                                                   
000324                 W0924E                                                   
000325     SKIP2                                                                
000326     ACCEPT DAGENS-DATUM  FROM DATE                                       
000327     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000328     .                                                                    
000329     EJECT                                                                
000330 B-SKAPA-R01-W0924A-FIL SECTION.                                          
000331                                                                          
000332     MOVE BAS-R01-DATA         TO W0924A-AREA                             
000333     PERFORM BA-SKAPA-KONTROLLPOST-R01                                    
000334                                                                          
000335     PERFORM S12-SKRIV-W0924A                                             
000336     .                                                                    
000337     EJECT                                                                
000338                                                                          
000339 BA-SKAPA-KONTROLLPOST-R01 SECTION.                                       
000340                                                                          
000341     MOVE ZERO                 TO KONTROLL-W-KONTROLL-IDDEL               
000342     MOVE SPACE                TO W-KONTROLL-POST                         
000343     MOVE WDG9-IDPTYP          TO KONTROLL-IDPTYP                         
000344     MOVE BAS-R01-IDARTNR      TO KONTROLL-SORTBGP                        
000345                                                                          
000346     MOVE BAS-R01-IDPTYP       TO R01-X-IDPTYP                            
000347     MOVE BAS-R01-IDARTNR      TO R01-X-IDARTNR                           
000348     MOVE BAS-R01-IDLEVNR      TO R01-X-IDLEVNR                           
000349     MOVE BAS-R01-IDLEVNR-SHIP TO R01-X-IDLEVNR-SHIP                      
000350     MOVE BAS-R01-IDSYSTEM     TO R01-X-IDSYSTEM                          
000351                                                                          
000352     MOVE R01-X-W213R01T       TO W-KONTROLL-POST                         
000353     .                                                                    
000354     EJECT                                                                
000355                                                                          
000356 C-SKAPA-R02-W0924B-FIL SECTION.                                          
000357                                                                          
000358     MOVE BAS-R02-DATA         TO W0924B-AREA                             
000359     PERFORM CA-SKAPA-KONTROLLPOST-R02                                    
000360     PERFORM S13-SKRIV-W0924B                                             
000361     .                                                                    
000362     EJECT                                                                
000363                                                                          
000364 CA-SKAPA-KONTROLLPOST-R02 SECTION.                                       
000365                                                                          
000366     MOVE ZERO                 TO KONTROLL-W-KONTROLL-IDDEL               
000367     MOVE SPACE                TO W-KONTROLL-POST                         
000368     MOVE WDG9-IDPTYP          TO KONTROLL-IDPTYP                         
000369     MOVE BAS-R02-IDLEVNR      TO KONTROLL-SORTBGP-X                      
000370                                                                          
000371                                                                          
000372     MOVE BAS-R02-IDPTYP       TO R02-X-IDPTYP                            
000373     MOVE BAS-R02-IDLEVNR      TO R02-X-IDLEVNR                           
000374     MOVE +1 TO PG-IX                                                     
000375     PERFORM UNTIL PG-IX > 8                                              
000376         MOVE BAS-R02-IDPLANGR-ANSK (PG-IX) TO                            
000377                               R02-X-IDPLANGR-ANSK(PG-IX)                 
000378         MOVE BAS-R02-IDANSK (PG-IX) TO R02-X-IDANSK (PG-IX)              
000379         ADD 1 TO PG-IX                                                   
000380     END-PERFORM                                                          
000381                                                                          
000382     MOVE R02-X-W213R02T       TO W-KONTROLL-POST                         
000383     .                                                                    
000384     EJECT                                                                
000385 D-SKAPA-R05-W0924E-FIL SECTION.                                          
000386                                                                          
000387     MOVE '144'                 TO R05-IDPTYP                             
000388     MOVE BAS-R05-IDARTNR       TO WS-IDARTNR-9                           
000389     MOVE WS-IDARTNR-9          TO R05-IDARTNR                            
000390     MOVE BAS-R05-IDFVARDE-NYTT TO WS-IDFVARDE                            
000391     MOVE WS-IDLKTO             TO R05-IDLKTO                             
000392                                                                          
000393     PERFORM DA-SKAPA-KONTROLLPOST-R05                                    
000394                                                                          
000395     PERFORM S16-SKRIV-W0924E                                             
000396     .                                                                    
000397     EJECT                                                                
000398                                                                          
000399 DA-SKAPA-KONTROLLPOST-R05 SECTION.                                       
000400                                                                          
000401     MOVE ZERO                  TO KONTROLL-W-KONTROLL-IDDEL              
000402     MOVE SPACE                 TO W-KONTROLL-POST                        
000403     MOVE WDG9-IDPTYP           TO KONTROLL-IDPTYP                        
000404     MOVE BAS-R05-IDARTNR       TO WS-IDARTNR-9                           
000405     MOVE WS-IDARTNR-9          TO KONTROLL-SORTBGP                       
000406                                                                          
000407     MOVE BAS-R05-IDPTYP        TO R05-X-IDPTYP                           
000408     MOVE BAS-R05-KDCLAGER      TO R05-X-KDCLAGER                         
000409     MOVE BAS-R05-IDARTNR       TO R05-X-IDARTNR                          
000410     MOVE BAS-R05-IDELMT        TO R05-X-IDELMT                           
000411     MOVE BAS-R05-KDTECKEN-NYTT TO R05-X-KDTECKEN-NYTT                    
000412     MOVE BAS-R05-IDFVARDE-NYTT TO R05-X-IDFVARDE-NYTT                    
000413     MOVE BAS-R05-KDTECKEN-BEF  TO R05-X-KDTECKEN-BEF                     
000414     MOVE BAS-R05-IDFVARDE-BEF  TO R05-X-IDFVARDE-BEF                     
000415                                                                          
000416     MOVE R05-X-W092R05T        TO W-KONTROLL-POST                        
000417     .                                                                    
000418     EJECT                                                                
000419                                                                          
000420 E-SKAPA-R17-W0924B-FIL SECTION.                                          
000421                                                                          
000422     MOVE BAS-R17-DATA         TO W0924B-AREA                             
000423     PERFORM EA-SKAPA-KONTROLLPOST-R17                                    
000424     PERFORM S13-SKRIV-W0924B                                             
000425     .                                                                    
000426     EJECT                                                                
000427                                                                          
000428 EA-SKAPA-KONTROLLPOST-R17 SECTION.                                       
000429                                                                          
000430     MOVE ZERO                  TO KONTROLL-W-KONTROLL-IDDEL              
000431     MOVE SPACE                 TO W-KONTROLL-POST                        
000432     MOVE BAS-R17-IDPTYP        TO KONTROLL-IDPTYP                        
000433     MOVE BAS-R17-IDLEVNR       TO KONTROLL-SORTBGP-X                     
000434                                                                          
000435     MOVE BAS-R17-IDPTYP        TO R17-X-IDPTYP                           
000436     MOVE BAS-R17-IDLEVNR       TO R17-X-IDLEVNR                          
000437     MOVE BAS-R17-KDBEHX        TO R17-X-KDBEHX                           
000438     MOVE BAS-R17-KVVECKOR-LT   TO R17-X-KVVECKOR-LT                      
000439     MOVE BAS-R17-KVVECKOR-AT   TO R17-X-KVVECKOR-AT                      
000440     MOVE BAS-R17-KVDAGAR-TTC1  TO R17-X-KVDAGAR-TTC1                     
000441     MOVE BAS-R17-KVDAGAR-TTC2  TO R17-X-KVDAGAR-TTC2                     
000442     MOVE BAS-R17-KDLEVTYP      TO R17-X-KDLEVTYP                         
000443     MOVE BAS-R17-KDGK          TO R17-X-KDGK                             
000444     MOVE BAS-R17-IDLPKOLL      TO R17-X-IDLPKOLL                         
000445     MOVE BAS-R17-FLRSADR       TO R17-X-FLRSADR                          
000446     MOVE BAS-R17-FLEMBPOL      TO R17-X-FLEMBPOL                         
000447     MOVE BAS-R17-KDSPRAK       TO R17-X-KDSPRAK                          
000448                                                                          
000449     MOVE R17-X-W213R17T       TO W-KONTROLL-POST                         
000450     .                                                                    
000451     EJECT                                                                
000452                                                                          
000453                                                                          
000454 F-SKAPA-R22-W0924C-FIL SECTION.                                          
000455                                                                          
000456     MOVE BAS-R22-DATA         TO W0924C-AREA                             
000457     PERFORM FA-SKAPA-KONTROLLPOST-R22                                    
000458     PERFORM S14-SKRIV-W0924C                                             
000459     .                                                                    
000460     EJECT                                                                
000461                                                                          
000462 FA-SKAPA-KONTROLLPOST-R22 SECTION.                                       
000463                                                                          
000464     MOVE ZERO                 TO KONTROLL-W-KONTROLL-IDDEL               
000465     MOVE SPACE                TO W-KONTROLL-POST                         
000466     MOVE R22-IDPTYP           TO KONTROLL-IDPTYP                         
000467     MOVE R22-IDARTNR          TO KONTROLL-SORTBGP                        
000468                                                                          
000469     MOVE BAS-R22-IDPTYP       TO R22-X-IDPTYP                            
000470     MOVE BAS-R22-IDARTNR      TO R22-X-IDARTNR                           
000471     MOVE BAS-R22-IDBEST       TO R22-X-IDBEST                            
000472     MOVE BAS-R22-IDLEVNR-BEST TO R22-X-IDLEVNR-BEST                      
000473     MOVE SPACE                TO R22-X-IDLEVNR-SHIP                      
000474     MOVE BAS-R22-TIBEST       TO R22-X-TIBEST                            
000475     MOVE BAS-R22-KVBEST       TO R22-X-KVBEST                            
000476     MOVE BAS-R22-KDBEH-BEST   TO R22-X-KDBEH-BEST                        
000477     MOVE BAS-R22-TENOT-BESTPRIS TO R22-X-TENOT-BESTPRIS                  
000478                                                                          
000479     MOVE R22-X-W212R22K       TO W-KONTROLL-POST                         
000480     .                                                                    
000481     EJECT                                                                
000482                                                                          
000483                                                                          
000484 G-SKAPA-R23-W0924C-FIL SECTION.                                          
000485                                                                          
000486     MOVE BAS-R23-DATA         TO W0924C-AREA                             
000487     PERFORM GA-SKAPA-KONTROLLPOST-R23                                    
000488     PERFORM S14-SKRIV-W0924C                                             
000489     .                                                                    
000490     EJECT                                                                
000491                                                                          
000492 GA-SKAPA-KONTROLLPOST-R23 SECTION.                                       
000493                                                                          
000494     MOVE ZERO                 TO KONTROLL-W-KONTROLL-IDDEL               
000495     MOVE SPACE                TO W-KONTROLL-POST                         
000496     MOVE R22-IDPTYP           TO KONTROLL-IDPTYP                         
000497     MOVE R22-IDARTNR          TO KONTROLL-SORTBGP                        
000498                                                                          
000499     MOVE BAS-R23-IDPTYP       TO R23-X-IDPTYP                            
000500     MOVE BAS-R23-IDARTNR      TO R23-X-IDARTNR                           
000501     MOVE BAS-R23-IDAVTAL      TO R23-X-IDBEST                            
000502     MOVE BAS-R23-IDLEVNR-AVT  TO R23-X-IDLEVNR-BEST                      
000503     MOVE BAS-R23-IDLEVNR-SHIP TO R23-X-IDLEVNR-SHIP                      
000504     MOVE BAS-R23-TIAVTAL      TO R23-X-TIBEST                            
000505     MOVE BAS-R23-KVAVTANT     TO R23-X-KVBEST                            
000506     MOVE BAS-R23-KDBEH-AVT    TO R23-X-KDBEH-BEST                        
000507     MOVE BAS-R23-TENOT-AVTPRIS TO R23-X-TENOT-BESTPRIS                   
000508                                                                          
000509     MOVE R23-X-W212R22K       TO W-KONTROLL-POST                         
000510                                                                          
000511                                                                          
000512     .                                                                    
000513     EJECT                                                                
000514                                                                          
000515 Z-FINIT SECTION.                                                         
000516     CLOSE W0924A                                                         
000517           W0924B                                                         
000518           W0924C                                                         
000519           W0924E                                                         
000520     SKIP2                                                                
000521     MOVE 'S' TO POSTSUM-OPKOD                                            
000522     CALL POSTSUM USING POSTSUM-PARM                                      
000523     .                                                                    
000524     EJECT                                                                
000525 S12-SKRIV-W0924A SECTION.                                                
000526     SKIP2                                                                
000527     WRITE W0924A-POST FROM W0924A-AREA                                   
000528                                                                          
000529     MOVE WDG9-IDPTYP TO POSTSUM-TRANSTYP                                 
000530     MOVE 'W0924A' TO POSTSUM-FDNAMN                                      
000531     MOVE 'W09242D2' TO POSTSUM-DDNAMN2                                   
000532     CALL POSTSUM USING POSTSUM-PARM                                      
000533     .                                                                    
000534     EJECT                                                                
000535 S13-SKRIV-W0924B SECTION.                                                
000536     SKIP2                                                                
000537     WRITE W0924B-POST FROM W0924B-AREA                                   
000538                                                                          
000539     MOVE WDG9-IDPTYP TO POSTSUM-TRANSTYP                                 
000540     MOVE 'W0924B' TO POSTSUM-FDNAMN                                      
000541     MOVE 'W09242D3' TO POSTSUM-DDNAMN2                                   
000542     CALL POSTSUM USING POSTSUM-PARM                                      
000543     .                                                                    
000544     EJECT                                                                
000545                                                                          
000546 S14-SKRIV-W0924C SECTION.                                                
000547     SKIP2                                                                
000548     WRITE W0924C-POST FROM W0924C-AREA                                   
000549                                                                          
000550     MOVE WDG9-IDPTYP TO POSTSUM-TRANSTYP                                 
000551     MOVE 'W0924C' TO POSTSUM-FDNAMN                                      
000552     MOVE 'W09242D4' TO POSTSUM-DDNAMN2                                   
000553     CALL POSTSUM USING POSTSUM-PARM                                      
000554     .                                                                    
000555     EJECT                                                                
000556                                                                          
000557 S16-SKRIV-W0924E SECTION.                                                
000558     SKIP2                                                                
000559     WRITE W0924E-POST FROM W0924E-AREA                                   
000560                                                                          
000561     MOVE WDG9-IDPTYP TO POSTSUM-TRANSTYP                                 
000562     MOVE 'W0924E' TO POSTSUM-FDNAMN                                      
000563     MOVE 'W09242D6' TO POSTSUM-DDNAMN2                                   
000564     CALL POSTSUM USING POSTSUM-PARM                                      
000565     .                                                                    
000566     EJECT                                                                
000567                                                                          
000568 S20-NOLLSTALL SECTION.                                                   
000569                                                                          
000570     MOVE SPACE                    TO  W0924A-AREA                        
000571                                       W0924B-AREA                        
000572                                       W0924C-AREA                        
000573                                       W0924E-AREA                        
000574      .                                                                   
000575      EJECT                                                               
000576* --- IMS SEKTIONER ---                                                   
000577     SKIP3                                                                
000578 IMS-GHN-WDG901 SECTION.                                                  
000579     SKIP2                                                                
000580     MOVE   'WLZZAD01 ' TO SSA1                                           
000581     MOVE '  GB' TO GODK-STATUSKODER                                      
000582     CALL CBLTDLI USING GHN ZZAD-PCB DLI-IO-AREA                          
000583     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
000584     PERFORM IMS-STATUSKONTROLL                                           
000585     .                                                                    
000586     SKIP3                                                                
000587     EJECT                                                                
000588 IMS-DLET-WDG901 SECTION.                                                 
000589                                                                          
000590     MOVE '  ' TO GODK-STATUSKODER                                        
000591     CALL CBLTDLI USING DLET ZZAD-PCB DLI-IO-AREA                         
000592     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
000593     PERFORM IMS-STATUSKONTROLL                                           
000594     ADD 1 TO UPD-RAEKNARE                                                
000595     .                                                                    
000596     EJECT                                                                
000597 IMS-STATUSKONTROLL SECTION.                                              
000598     SKIP2                                                                
000599     SET STATUS-IX TO 1                                                   
000600     SEARCH GODK-STATUS                                                   
000601       AT END                                                             
000602         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
000603         DISPLAY FELTEXT                                                  
000604         CALL FELLOG                                                      
000605       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000606         CONTINUE                                                         
000607     END-SEARCH                                                           
000608     .                                                                    
