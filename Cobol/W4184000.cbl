000001 ID DIVISION.                                                             
000002 PROGRAM-ID.                 W4184000.                                    
000003*AUTHOR.                     INGRID DANIELSSON.                           
000004*                            W418    REDIGERING AV LISTOR.                
000005 DATE-WRITTEN.               DEC     1977.                                
000006 DATE-COMPILED.                                                           
000007*  OMARBETAT                 NOV     1994 AV JAN-ERIK FRANTZEN.           
000008*  OMARBETAT FÖR LEVANM.PROJ SEP     1995 AV JAN-ERIK FRANTZEN.           
000009*  OMARBETAT FÖR SAP/R3.PROJ MAJ     1998 AV SUSANNE OLSSON.              
000010*                                                                         
000011*  E-TRACKER : 2420793  2005-08-31                                        
000012*  E-TRACKER : 2072166  2005-09-15                                        
000013*  E-TRACKER : 1658417  2006-03-01                                        
000014*                                                                         
000015*  FUNKTION:                                                              
000016*  PROGRAMMET ÄR ETT  I M S - PROGRAM SOM                                 
000017*       SKRIVER EN FIL TILL   INNEHÅLLANDE OREDIGERADE POSTER MED         
000018*                             KREDITNOTADATA.                             
000019*                                                                         
000020*       SKRIVER EN FIL TILL   INNEHÅLLANDE OREDIGERADE POSTER MED         
000021*                             RETURTILLSTÅNDSDATA.                        
000022*                                                                         
000023*       SKRIVER EN FIL TILL  INNEHÅLLANDE INTRASTATPOSTER MED INFO        
000024*                            VID UNDERLEVERANS OCH INTERNUPPACKN.         
000025*                                                                         
000026*       SKRIVER EN FIL TILL  INNEHÅLLANDE INTRASTATPOSTER MED INFO        
000027*                            VID RETURER.                                 
000028*                                                                         
000029*                                                                         
000030*  ÄNDRING FEB-92: REPORT-WRITER ERSATT AV SEKVENSFILER                   
000031*                                                                         
000032*  INDATA:                                                                
000033*                                                                         
000034*          W41835  PT712  HUVUD KREDITNOTA                                
000035*                  PT713  RADPOST    -*-                                  
000036*                                                                         
000037*                  PT717  HUVUD RETURTILLSTÅND                            
000038*                  PT718  RADPOST     -*-                                 
000039*                                                                         
000040*  UTDATA: W41850  KREDITNÖTTER OCH RETILL  CDC ,SDC,NDC-JA,NDC-AU        
000041*                                           DDC OCH LDC-SE                
000042*                                                                         
000043*                  PT76A  HUVUDPOST CDC  RETILL                           
000044*                  PT76B  RADPOST   CDC  RETILL                           
000045*                                                                         
000046*                  PT78A  HUVUDPOST KREDITNÖTTER CDC + SDC + DDC +        
000047*                                            LDC-SE + NDC-JA/AU           
000048*                  PT78B  RADPOST   KREDITNÖTTER CDC + SDC + DDC +        
000049*                                            LDC-SE + NDC-JA/AU           
000050*                                                                         
000051*  DATABASER:  WLBENA   WDD3   BENÄMNINGSREGISTRET                        
000052*              WLARTC   WDK6   ARTIKELREGISTER                            
000053*              WLGMTA   WDB2   KUNDREGISTRET                              
000054*              WDB101          PAS KUNDREGISTRET                          
000055*                                                                         
000056*  SUBPROGR:   DATKORT                                                    
000057*              CBLTDLI                                                    
000058*              FELLOG                                                     
000059*              WDECEDIT                                                   
000060     EJECT                                                                
000061 ENVIRONMENT DIVISION.                                                    
000062 INPUT-OUTPUT SECTION.                                                    
000063 FILE-CONTROL.                                                            
000064     SELECT   W41835   ASSIGN TO  W41840D1.                               
000065     SELECT   W41849   ASSIGN TO  W41840D2.                               
000066     SELECT   W41850   ASSIGN TO  W41840D3.                               
000067     SELECT   W41854   ASSIGN TO  W41840D4.                               
000068     SELECT   W41848   ASSIGN TO  W41840D5.                               
000069     EJECT                                                                
000070 DATA DIVISION.                                                           
000071 FILE SECTION.                                                            
000072 FD  W41835                                                               
000073     RECORDING V                                                          
000074     BLOCK 0.                                                             
000075 01  INPOST.                                                              
000076     03  IN-IDPTYP      PIC X(3).                                         
000077     03  IN-IDDISTR     PIC S9(5)   COMP-3.                               
000078     03     FILLER      PIC X(287).                                       
000079     EJECT                                                                
000080                                                                          
000081 FD  W41849                                                               
000082     RECORDING V                                                          
000083     BLOCK 0.                                                             
000084*01  W41876A  -COPY W41876A -L -PRE UT-                                   
000085     EJECT                                                                
000086*01  W41876B  -COPY W41876B -L -PRE UT-                                   
000087     EJECT                                                                
000088                                                                          
000089 FD  W41850                                                               
000090     RECORDING V                                                          
000091     BLOCK 0.                                                             
000092*01  W41878A  -COPY W41878A -L -PRE UT-                                   
000093     EJECT                                                                
000094*01  W41878B  -COPY W41878B -L -PRE UT-                                   
000095     EJECT                                                                
000096                                                                          
000097 FD  W41854                                                               
000098     LABEL RECORD   STANDARD                                              
000099     RECORDING      F                                                     
000100     BLOCK CONTAINS 0.                                                    
000101     SKIP2                                                                
000102*01  POST -COPY W475INT    -PRE INTRA- -L.                                
000103     EJECT                                                                
000104                                                                          
000105 FD  W41848                                                               
000106     LABEL RECORD   STANDARD                                              
000107     RECORDING      F                                                     
000108     BLOCK CONTAINS 0.                                                    
000109     SKIP2                                                                
000110*01  POST -COPY W61171     -PRE INTRA2- -L.                               
000111     EJECT                                                                
000112 WORKING-STORAGE SECTION.                                                 
000113*    -- CHECKED BY WY2000                                                 
000114     SKIP3                                                                
000115 77  IDPGM                     PIC X(8)          VALUE 'W4184000'.        
000116 77  FELTEXT-STR               PIC X(80)         VALUE SPACE.             
000117 77  FLTA                      PIC X(6)          VALUE 'W41840'.          
000118 77  FLTB                      PIC X(6)          VALUE 'WDATUM'.          
000119 77  JA                        PIC X             VALUE 'J'.               
000120 77  NEJ                       PIC X             VALUE 'N'.               
000121 77  W-VALUTA                  PIC X(3)          VALUE SPACE.             
000122 77  W-KURS-LOC                PIC  9(6)V9(5).                            
000123 77  W-KURS-SDC                PIC  9(6)V9(5).                            
000124 77  W-REVALUTA-LOC            PIC  9(3).                                 
000125 77  W-REVALUTA-SDC            PIC  9(3).                                 
000126 77  WS-ADRESS-1               PIC X(27).                                 
000127 77  WS-ADRESS-2               PIC X(27).                                 
000128 77  WS-ADRESS-3               PIC X(27).                                 
000129 77  WS-ADRESS-4               PIC X(27).                                 
000130 77  W-DATE-AAMM               PIC 9(4)    VALUE ZERO.                    
000131 77  WS-KDVALISO-HUV           PIC X(3)    VALUE 'SEK'.                   
000132 77  GODK-IX                   PIC S9(3)   VALUE +0   COMP-3.             
000133 77  MAX-GODK-IX               PIC S9(3)   VALUE +5   COMP-3.             
000134                                                                          
000135 01  DATUMKORT.                                                           
000136     03  FILLER          PIC X(11).                                       
000137     03  DATUM           PIC 9(6).                                        
000138     03  FILLER          PIC X(63).                                       
000139     EJECT                                                                
000140 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000141 01  FILLER REDEFINES DAGENS-DATUM.                                       
000142     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000143     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000144     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000145     EJECT                                                                
000146 01  SUBPROGRAM.                                                          
000147     03  DATKORT         PIC X(8)           VALUE 'DATKORT'.              
000148     03  CBLTDLI         PIC X(8)           VALUE 'CBLTDLI'.              
000149     03  FELLOG          PIC X(8)           VALUE 'FELLOG  '.             
000150     03  W418OKOD        PIC X(8)           VALUE 'W418OKOD'.             
000151     03  POSTSUM         PIC X(8)           VALUE 'POSTSUM'.              
000152     03  W510CURR        PIC X(8)           VALUE 'W510CURR'.             
000153     EJECT                                                                
000154*                                                                         
000155*    ---  LÄNKAREA TILL W418OKOD                                          
000156     SKIP3                                                                
000157*    03 -COPY W418OKOD           -PRE OKOD-.                              
000158     EJECT                                                                
000159                                                                          
000160*    --- PARAMETRAR TILL POSTSUM                                          
000161*                                                                         
000162*01  -COPY W0005   -PRE  POSTSUM-                                         
000163     EJECT                                                                
000164*    --- PARAMETRAR TILL W510CURR                                         
000165*                                                                         
000166*01  -COPY W510CURR                                                       
000167     EJECT                                                                
000168   03  GODK-STATUSKODER.                                                  
000169     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000170     SKIP2                                                                
000171*                                                                         
000172   03  SSA1                 PIC X(64).                                    
000173   03  SSA2                 PIC X(64).                                    
000174*                                                                         
000175 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
000176*01  FILLER  -COPY WWDIST13 -RED TEST-IDDISTR.                            
000177     EJECT                                                                
000178*01  FILLER  -COPY WWDIST72 -RED TEST-IDDISTR.                            
000179     EJECT                                                                
000180                                                                          
000181 01  FILLER                 PIC X(16)  VALUE 'BYTES-ARTIKEL'.             
000182                                                                          
000183 01  FILLER                 PIC X(16)  VALUE 'EG-LAND      '.             
000184 01  TEST-IDLANDX2          PIC X(2).                                     
000185*01  FILLER  -COPY WWLANDX2 -RED TEST-IDLANDX2.                           
000186     EJECT                                                                
000187                                                                          
000188 01  FILLER                   PIC X(24)  VALUE 'EU-LEV-TAB-START'.        
000189     SKIP2                                                                
000190 01  TABELL-INTRASTAT.                                                    
000191   03  EU-LEV-TAB.                                                        
000192     05  FILLER               PIC X(8)    VALUE '00069 SE'.               
000193     05  FILLER               PIC X(8)    VALUE '00999 DK'.               
000194     05  FILLER               PIC X(8)    VALUE '01099 FI'.               
000195     05  FILLER               PIC X(8)    VALUE '01279 BE'.               
000196     05  FILLER               PIC X(8)    VALUE '01339 GB'.               
000197     05  FILLER               PIC X(8)    VALUE '01479 FR'.               
000198     05  FILLER               PIC X(8)    VALUE '01679 NL'.               
000199     05  FILLER               PIC X(8)    VALUE '01879 IT'.               
000200     05  FILLER               PIC X(8)    VALUE '02179 ES'.               
000201     05  FILLER               PIC X(8)    VALUE '02259 DE'.               
000202     05  FILLER               PIC X(8)    VALUE '02359 AT'.               
000203     05  FILLER               PIC X(8)    VALUE '02679 EE'.               
000204     05  FILLER               PIC X(8)    VALUE '02879 PL'.               
000205     03  REEULEV   REDEFINES EU-LEV-TAB.                                  
000206         05  EU-LEV OCCURS 13                                             
000207             ASCENDING KEY EU-IDDISTR                                     
000208             INDEXED BY EU-IX.                                            
000209             07  EU-IDDISTR   PIC 9(5).                                   
000210             07  FILLER       PIC X(1).                                   
000211             07  EU-IDLANDX2  PIC X(2).                                   
000212     EJECT                                                                
000213 01  SWITCHAR.                                                            
000214     03  EOF-SW               PIC X       VALUE 'N'.                      
000215         88  EOF-W41835                   VALUE 'J'.                      
000216                                                                          
000217 01  W-FORSAKRAN.                                                         
000218     03  W-FORSAKRAN-DEL-1    PIC X(38)   VALUE SPACE.                    
000219     03  W-FORSAKRAN-DEL-2    PIC X(34)   VALUE SPACE.                    
000220******************************************************************        
000221*                             ***  NEDANSTÅENDE ANVÄNDS FÖR ATT **        
000222*                             ***  LÄGGA UT TEXTER LÄNGST TILL  **        
000223*                             ***  VÄNSTER PÅ FAKTURARADER.     **        
000224*                             ***  OLIKA TEXTER FÖR BYTES RESP  **        
000225*                             ***  LEVERANSANMÄRKNINGAR         **        
000226 01  LISTA3-INLEDN.                                                       
000227     03  L3-LEVANM.                                                       
000228         05  L3-KDFAKTYP      PIC X.                                      
000229         05  L3-IDFAKT        PIC Z(7).                                   
000230         05  L3-TIFAKT        PIC Z(6).                                   
000231     03  L3-BYTES REDEFINES L3-LEVANM.                                    
000232         05  L3-BEKUNDREF     PIC X(14).                                  
000233     EJECT                                                                
000234 01  IFYLLDA-FALT.                                                        
000235     03  W-SIDA-01          PIC S9(5)      COMP-3   VALUE ZERO.           
000236     03  W-RADR-01          PIC S9(5)      COMP-3   VALUE ZERO.           
000237     03  W-SIDA-02          PIC S9(5)      COMP-3   VALUE ZERO.           
000238     03  W-RADR-02          PIC S9(5)      COMP-3   VALUE ZERO.           
000239     03  W-SIDA-03-713      PIC S9(5)      COMP-3   VALUE ZERO.           
000240     03  W-RADR-03-713      PIC S9(5)      COMP-3   VALUE ZERO.           
000241     03  W-ANT-01-SIDOR     PIC S9(5)      COMP-3   VALUE ZERO.           
000242     03  W-ANT-02-SIDOR     PIC S9(5)      COMP-3   VALUE ZERO.           
000243     03  W-ANT-03-SIDOR     PIC S9(5)      COMP-3   VALUE ZERO.           
000244     03  W-KDERS-ASTERISK   PIC X                   VALUE SPACE.          
000245     03  W-BEART            PIC X(15)               VALUE SPACE.          
000246     03  W-SUKRENOT         PIC S9(7)V99   COMP-3   VALUE ZERO.           
000247     03  W-SUMMA-1          PIC S9(7)V99   COMP-3   VALUE ZERO.           
000248     03  W-SUMMA-3          PIC S9(7)V99   COMP-3   VALUE ZERO.           
000249     03  W-SUMMA-4          PIC S9(7)V99   COMP-3   VALUE ZERO.           
000250     EJECT                                                                
000251*            * * * * * * * * * * *                                        
000252*            *   I N F I L E N   *                                        
000253*            * * * * * * * * * * *                                        
000254******************************************************************        
000255*01  POST         -COPY W418712     -PRE  712-                            
000256     EJECT                                                                
000257******************************************************************        
000258*01  POST         -COPY W418713     -PRE  713-                            
000259     EJECT                                                                
000260******************************************************************        
000261*01  POST         -COPY W418717     -PRE  717-                            
000262     EJECT                                                                
000263******************************************************************        
000264*01  POST         -COPY W418718     -PRE  718-                            
000265     EJECT                                                                
000266******************************************************************        
000267*              * * * * * * * * * * * * *                                  
000268*              *   UT-FILER            *                                  
000269*              * * * * * * * * * * * * *                                  
000270******************************************************************        
000271*01  POST         -COPY W41876A     -PRE  76A-                            
000272     EJECT                                                                
000273*01  POST         -COPY W41876B     -PRE  76B-                            
000274     EJECT                                                                
000275*01  POST         -COPY W41878A     -PRE  78A-                            
000276     EJECT                                                                
000277*01  POST         -COPY W41878B     -PRE  78B-                            
000278     EJECT                                                                
000279******************************************************************        
000280*       UTAREA INTRASTATUPPGIFTER TILL VOLVO TRANSPORT           *        
000281******************************************************************        
000282     SKIP2                                                                
000283 01  FILLER            PIC X(16) VALUE 'UTAREA INTRASTAT'.                
000284 01  INTRA-UTAREA.                                                        
000285*    03  -COPY W475INT      -PRE INTRA-                                   
000286     EJECT                                                                
000287******************************************************************        
000288*       UTAREA INTRASTATUPPGIFTER TILL VOLVO TRANSPORT           *        
000289******************************************************************        
000290     SKIP2                                                                
000291 01  FILLER            PIC X(18) VALUE 'UTAREA-2 INTRASTAT'.              
000292 01  INTRA2-UTAREA.                                                       
000293*    03  -COPY W61171       -PRE INTRA2-                                  
000294     EJECT                                                                
000295*      ************************************************                   
000296*      ******  ARBETSAREOR TILL IMS-SECTIONERNA  ******                   
000297*      ************************************************                   
000298     SKIP3                                                                
000299 01  IMS-WS.                                                              
000300     03  FILLER        PIC X(16)     VALUE '     IMS-WS     '.            
000301                                                                          
000302 01  STATUS-WS         PIC XX.                                            
000303     88  SEGMENT-FINNS               VALUE '  '.                          
000304     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
000305                                                                          
000306 01  NYCKLAR-TILL-DLI.                                                    
000307     03  W-IDARTNR-X.                                                     
000308         05 W-IDARTNR            PIC S9(9)  COMP-3   VALUE ZERO.          
000309                                                                          
000310     03 W-IDGMT-X.                                                        
000311         05 W-IDDISTR-WDB2       PIC S9(5)    COMP-3.                     
000312         05 W-IDKUNDNR-WDB2      PIC S9(7)    COMP-3.                     
000313                                                                          
000314     03 W-IDGMT-MIN-X.                                                    
000315         05 W-IDDISTR-WDB2-MIN   PIC S9(5)    COMP-3.                     
000316         05 W-IDKUNDNR-WDB2-MIN  PIC S9(7)    COMP-3.                     
000317                                                                          
000318     03 W-IDGMT-MAX-X.                                                    
000319         05 W-IDDISTR-WDB2-MAX   PIC S9(5)    COMP-3.                     
000320         05 W-IDKUNDNR-WDB2-MAX  PIC S9(7)    COMP-3.                     
000321                                                                          
000322     03  W-WDB101KY-X.                                                    
000323         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
000324         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
000325                                                                          
000326                                                                          
000327     03  W-IDSKYLT-X.                                                     
000328         05 W-IDSKYLT            PIC X(3).                                
000329                                                                          
000330     03 W-KDSEGKEY-X.                                                     
000331        05 W-KDSEGKEY            PIC  X(1)   VALUE '1'.                   
000332                                                                          
000333     03  W-IDDC-B6-X.                                                     
000334         05 W-IDDC-B6                  PIC X(2).                          
000335                                                                          
000336     EJECT                                                                
000337*01    -COPY    W0003                                                     
000338     EJECT                                                                
000339 01  DLI-IO-AREOR        PIC X(24)   VALUE '****--- DLI-IO-AREOR'.        
000340                                                                          
000341 01  DLI-IO-AREA-WDB101.                                                  
000342*    03  WDB101   -COPY WDB101.                                           
000343     EJECT                                                                
000344 01  DLI-IO-AREA-WDB201.                                                  
000345*    03 WLGMTA01  -COPY WDB201.                                           
000346     EJECT                                                                
000347 01  DLI-IO-AREA-WDD301.                                                  
000348*    03  WLBENA01 -COPY WDD301.                                           
000349     EJECT                                                                
000350 01  DLI-IO-AREA-WDD311.                                                  
000351*    03  WLBENA11 -COPY WDD311.                                           
000352     EJECT                                                                
000353 01  DLI-IO-AREA-WDK601.                                                  
000354*    03  WLARTC01 -COPY WDK601.                                           
000355     EJECT                                                                
000356 01  DLI-IO-AREA-WDK611.                                                  
000357*    03  WLARTC11 -COPY WDK611.                                           
000358     EJECT                                                                
000359 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000360 01   DLI-IO-AREA-B601.                                                   
000361*     03  -COPY WDB601                                                    
000362     EJECT                                                                
000363 LINKAGE SECTION.                                                         
000364*01    -COPY  W0008      -PRE  GMTA-                                      
000365     05  GMTA-KONKAT-NKL        PIC X.                                    
000366     EJECT                                                                
000367*01    -COPY  W0008      -PRE  WDG2-                                      
000368     05  WDG2-KONKAT-NKL        PIC X.                                    
000369                                                                          
000370*01    -COPY  W0008      -PRE  ARTC-                                      
000371     05  ARTC-KONKAT-NKL        PIC X.                                    
000372     EJECT                                                                
000373*01    -COPY  W0008      -PRE  BENA-                                      
000374     05  FILLER                 PIC X.                                    
000375     EJECT                                                                
000376*01    -COPY  W0008      -PRE  WDB1-                                      
000377     05  FILLER                 PIC X.                                    
000378     EJECT                                                                
000379*01    -COPY  W0008      -PRE  WDB6-                                      
000380     05  FILLER                 PIC X.                                    
000381     EJECT                                                                
000382 PROCEDURE DIVISION   USING GMTA-PCB WDG2-PCB                             
000383                            ARTC-PCB BENA-PCB WDB1-PCB WDB6-PCB.          
000384                                                                          
000385 STYR  SECTION.                                                           
000386                                                                          
000387     ENTRY 'DLITCBL' USING GMTA-PCB WDG2-PCB                              
000388                           ARTC-PCB BENA-PCB WDB1-PCB WDB6-PCB.           
000389     PERFORM A-INIT                                                       
000390     PERFORM S01-LAES-INFIL                                               
000391                                                                          
000392     PERFORM UNTIL EOF-W41835                                             
000393       PERFORM B-BEHANDLA                                                 
000394       PERFORM S01-LAES-INFIL                                             
000395     END-PERFORM                                                          
000396     PERFORM Z-FINIT                                                      
000397                                                                          
000398     MOVE ZERO TO RETURN-CODE                                             
000399     GOBACK                                                               
000400     .                                                                    
000401     SKIP2                                                                
000402 A-INIT  SECTION.                                                         
000403                                                                          
000404     MOVE LOW-VALUE           TO W-IDGMT-MIN-X                            
000405                                 W-IDGMT-X                                
000406                                                                          
000407     MOVE HIGH-VALUE          TO W-IDGMT-MAX-X                            
000408                                                                          
000409     MOVE SPACE TO W-FORSAKRAN                                            
000410     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
000411                                                                          
000412     CALL DATKORT USING FLTA FLTB DATUMKORT                               
000413     MOVE DATUM TO DAGENS-DATUM                                           
000414     MOVE DAGENS-DATUM-AAR    TO W-DATE-AAMM(1:2)                         
000415     MOVE DAGENS-DATUM-MAANAD TO W-DATE-AAMM(3:2)                         
000416     OPEN INPUT   W41835                                                  
000417          OUTPUT  W41848 W41849 W41850 W41854                             
000418     .                                                                    
000419     EJECT                                                                
000420 B-BEHANDLA SECTION.                                                      
000421                                                                          
000422     MOVE IN-IDDISTR             TO TEST-IDDISTR                          
000423     EVALUATE IN-IDPTYP                                                   
000424     WHEN '712'                                                           
000425       MOVE INPOST               TO 712-POST                              
000426       MOVE 712-IDDISTR          TO W-IDDISTR-WDB2                        
000427                                    W-IDDISTR-WDB2-MIN                    
000428                                    W-IDDISTR-WDB2-MAX                    
000429       MOVE 712-IDKUNDNR         TO W-IDKUNDNR-WDB2                       
000430                                                                          
000431       PERFORM S20-LAES-OCH-SPARA-KUNDREG                                 
000432       PERFORM S40-HAEMTA-VALUTADATA                                      
000433       PERFORM S100-HUVUDPOST-KNOTA-712                                   
000434                                                                          
000435       MOVE +1                   TO W-SIDA-03-713                         
000436       ADD +1                    TO W-ANT-03-SIDOR                        
000437     WHEN '713'                                                           
000438       MOVE INPOST               TO 713-POST                              
000439       MOVE 713-IDDISTR          TO W-IDDISTR-WDB2                        
000440                                    W-IDDISTR-WDB2-MIN                    
000441                                    W-IDDISTR-WDB2-MAX                    
000442       MOVE 713-IDKUNDNR         TO W-IDKUNDNR-WDB2                       
000443                                                                          
000444       PERFORM S20-LAES-OCH-SPARA-KUNDREG                                 
000445       PERFORM S30-LAES-OCH-SPARA-ARTIKELREG                              
000446                                                                          
000447       MOVE 713-KDFAKTYP         TO L3-KDFAKTYP                           
000448       MOVE 713-IDFAKT           TO L3-IDFAKT                             
000449       MOVE 713-TIFAKT           TO L3-TIFAKT                             
000450                                                                          
000451       COMPUTE W-SUMMA-3 ROUNDED =                                        
000452                        713-KVKREANT * 713-PRARTBTO                       
000453       MOVE W-SUMMA-3            TO W-SUMMA-4                             
000454       PERFORM S90-RADPOST-KNOTA-713                                      
000455                                                                          
000456       IF W-RADR-03-713 = 49                                              
000457         ADD +1                  TO W-SIDA-03-713                         
000458         ADD +1                  TO W-ANT-03-SIDOR                        
000459         MOVE +18                TO W-RADR-03-713                         
000460       END-IF                                                             
000461     WHEN '717'                                                           
000462       MOVE INPOST               TO 717-POST                              
000463       MOVE 717-IDDISTR          TO W-IDDISTR-WDB2                        
000464                                    W-IDDISTR-WDB2-MIN                    
000465                                    W-IDDISTR-WDB2-MAX                    
000466       MOVE 717-IDKUNDNR         TO W-IDKUNDNR-WDB2                       
000467       IF DIST72-EJ-PRIS                                                  
000468         MOVE ZERO               TO 717-REEMBHNT                          
000469                                    717-PRFRAKT                           
000470                                    717-PRLEGKST                          
000471                                    717-PRFOERS                           
000472                                    717-RELANDCO                          
000473       END-IF                                                             
000474       PERFORM S20-LAES-OCH-SPARA-KUNDREG                                 
000475       PERFORM S80-HUVUDPOST-RETUR                                        
000476                                                                          
000477       MOVE +1                   TO W-SIDA-01                             
000478       ADD +1                    TO W-ANT-01-SIDOR                        
000479     WHEN '718'                                                           
000480       MOVE INPOST               TO 718-POST                              
000481       MOVE 718-IDDISTR          TO W-IDDISTR-WDB2                        
000482                                    W-IDDISTR-WDB2-MIN                    
000483                                    W-IDDISTR-WDB2-MAX                    
000484       MOVE 718-IDKUNDNR         TO W-IDKUNDNR-WDB2                       
000485       PERFORM S20-LAES-OCH-SPARA-KUNDREG                                 
000486       PERFORM S30-LAES-OCH-SPARA-ARTIKELREG                              
000487                                                                          
000488       IF CLAG-KDERS NOT = +0                                             
000489         MOVE '*'                TO W-KDERS-ASTERISK                      
000490       ELSE                                                               
000491         MOVE SPACE              TO W-KDERS-ASTERISK                      
000492       END-IF                                                             
000493                                                                          
000494       IF DIST72-EJ-PRIS                                                  
000495         MOVE ZERO               TO   W-SUMMA-1                           
000496                                    718-PRARTBTO                          
000497       ELSE                                                               
000498          COMPUTE W-SUMMA-1 ROUNDED = 718-KVLEVANM * 718-PRARTBTO         
000499       END-IF                                                             
000500       PERFORM S70-RADPOST-RETUR                                          
000501                                                                          
000502       IF W-RADR-01 = 30                                                  
000503         ADD +1                  TO W-SIDA-01                             
000504         ADD +1                  TO W-ANT-01-SIDOR                        
000505         MOVE +10                TO W-RADR-01                             
000506       END-IF                                                             
000507     END-EVALUATE                                                         
000508     .                                                                    
000509     EJECT                                                                
000510 Z-FINIT SECTION.                                                         
000511                                                                          
000512     CLOSE  W41835 W41848 W41849 W41850 W41854                            
000513                                                                          
000514     MOVE 'S' TO POSTSUM-OPKOD                                            
000515     CALL POSTSUM USING POSTSUM-PARM                                      
000516     .                                                                    
000517     EJECT                                                                
000518 S01-LAES-INFIL SECTION.                                                  
000519                                                                          
000520     READ  W41835                                                         
000521     AT END                                                               
000522        MOVE 'J'                  TO EOF-SW                               
000523     NOT AT END                                                           
000524        MOVE 'IN'                 TO POSTSUM-TRANSTYP                     
000525        MOVE 'W41835'             TO POSTSUM-FDNAMN                       
000526        MOVE 'W41840D1'           TO POSTSUM-DDNAMN2                      
000527        CALL POSTSUM USING POSTSUM-PARM                                   
000528     END-READ                                                             
000529     .                                                                    
000530     EJECT                                                                
000531 S20-LAES-OCH-SPARA-KUNDREG      SECTION.                                 
000532     SKIP3                                                                
000533                                                                          
000534     PERFORM IMS-GET-WLGMTA01-UNIK                                        
000535     IF SEGMENT-FINNS                                                     
000536        CONTINUE                                                          
000537     ELSE                                                                 
000538        PERFORM IMS-GET-WLGMTA01                                          
000539     END-IF                                                               
000540     MOVE GMT-IDPARTNR            TO W-WDB1-IDPARTNR                      
000541     MOVE GMT-IDFTG               TO W-WDB1-IDFTG                         
000542                                                                          
000543     PERFORM IMS-GU-WDB101                                                
000544     IF (IN-IDPTYP = '717' OR '718') AND DIST13-SVERIGE                   
000545       MOVE GMT-BEGMT-RAD1        TO WS-ADRESS-1                          
000546       MOVE GMT-ADGMT-GATA        TO WS-ADRESS-2                          
000547       MOVE GMT-ADGMT-PADR        TO WS-ADRESS-3                          
000548       MOVE GMT-ADGMT-LAND        TO WS-ADRESS-4                          
000549     ELSE                                                                 
000550       MOVE BET-BEBETRAD-1        TO WS-ADRESS-1                          
000551       MOVE BET-BEBETRAD-2        TO WS-ADRESS-2                          
000552       MOVE BET-ADBETRAD-1        TO WS-ADRESS-3                          
000553       MOVE BET-ADBETRAD-2        TO WS-ADRESS-4                          
000554     END-IF                                                               
000555     .                                                                    
000556     EJECT                                                                
000557 S30-LAES-OCH-SPARA-ARTIKELREG         SECTION.                           
000558                                                                          
000559     IF IN-IDPTYP = '713'                                                 
000560       MOVE 713-IDARTNR           TO W-IDARTNR                            
000561     ELSE                                                                 
000562       MOVE 718-IDARTNR           TO W-IDARTNR                            
000563     END-IF                                                               
000564                                                                          
000565     MOVE GMT-IDSKYLT             TO W-IDSKYLT                            
000566                                                                          
000567     PERFORM IMS-BENA-LAS-ROTSEG                                          
000568     IF SEGMENT-FINNS                                                     
000569        PERFORM IMS-BENA-LAS-TEXTSEG                                      
000570     END-IF                                                               
000571                                                                          
000572     IF SEGMENT-FINNS                                                     
000573        MOVE TEXT-BEART           TO W-BEART                              
000574     ELSE                                                                 
000575        MOVE SPACE                TO W-BEART                              
000576     END-IF                                                               
000577*------------------------------------------ WDK6                          
000578                                                                          
000579     PERFORM IMS-GU-WLARTC01                                              
000580     IF ART-KDERS-UTG = 0                                                 
000581        PERFORM IMS-GNP-WLARTC11                                          
000582     END-IF                                                               
000583     .                                                                    
000584     EJECT                                                                
000585 S40-HAEMTA-VALUTADATA            SECTION.                                
000586                                                                          
000587     MOVE W-DATE-AAMM             TO CURR-TIAAMM                          
000588     MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                    
000589     MOVE 'M'                     TO CURR-KDVALTYP                        
000590                                                                          
000591     MOVE BET-KDVALISO            TO CURR-KDVALISO-ROW                    
000592                                                                          
000593     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
000594     IF CURR-KDSVAR = ' '                                                 
000595        CONTINUE                                                          
000596     ELSE                                                                 
000597        MOVE 1                    TO CURR-PRKURS-NEW                      
000598                                     CURR-REVALUTA-TO                     
000599     END-IF                                                               
000600     COMPUTE W-KURS-LOC = CURR-PRKURS-NEW * CURR-REVALUTA-TO              
000601     MOVE CURR-KDVALISO-ROW       TO W-VALUTA                             
000602     MOVE CURR-REVALUTA-TO        TO W-REVALUTA-LOC                       
000603                                                                          
000604     MOVE 712-IDDC                TO W-IDDC-B6                            
000605     PERFORM IMS-GU-WDB601                                                
000606     MOVE DCS-KDVALISO            TO CURR-KDVALISO-ROW                    
000607                                                                          
000608     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
000609     IF CURR-KDSVAR = ' '                                                 
000610        CONTINUE                                                          
000611     ELSE                                                                 
000612        MOVE 1                    TO CURR-PRKURS-NEW                      
000613                                     CURR-REVALUTA-TO                     
000614     END-IF                                                               
000615     COMPUTE W-KURS-SDC = CURR-PRKURS-NEW * CURR-REVALUTA-TO              
000616     MOVE CURR-REVALUTA-TO        TO W-REVALUTA-SDC                       
000617     .                                                                    
000618     EJECT                                                                
000619 S70-RADPOST-RETUR SECTION.                                               
000620                                                                          
000621     MOVE '76B'                   TO 76B-IDPTYP                           
000622     MOVE 718-IDDISTR             TO 76B-IDDISTR                          
000623     MOVE 718-IDKUNDNR            TO 76B-IDKUNDNR                         
000624     MOVE 718-IDRAPPNR            TO 76B-IDRAPPNR                         
000625     MOVE 718-IDDC                TO 76B-IDDC                             
000626     MOVE 718-IDARTNR             TO 76B-IDARTNR                          
000627     MOVE 718-IDORDNR             TO 76B-IDORDNR                          
000628     MOVE 718-IDRADNR             TO 76B-IDRADNR                          
000629     MOVE W-BEART                 TO 76B-BEART                            
000630     MOVE 718-KVLEVANM            TO 76B-KVLEVANM                         
000631     MOVE 718-KDANMORS            TO 76B-KDANMORS                         
000632     MOVE ART-REKSIFFR            TO 76B-REKSIFFR                         
000633     MOVE ZERO                    TO 76B-KDERS                            
000634     MOVE 718-PRARTBTO            TO 76B-PRARTBTO                         
000635     MOVE 718-PRARTBTO-LOC        TO 76B-PRARTBTO-LOC                     
000636     PERFORM S151-SKRIV-W41849-RAD                                        
000637     MOVE SPACE                   TO 76B-POST                             
000638     ADD 1                        TO W-RADR-01                            
000639     .                                                                    
000640     EJECT                                                                
000641 S80-HUVUDPOST-RETUR SECTION.                                             
000642                                                                          
000643     MOVE '76A'                   TO 76A-IDPTYP                           
000644     MOVE 717-IDDISTR             TO 76A-IDDISTR                          
000645     MOVE 717-IDKUNDNR            TO 76A-IDKUNDNR                         
000646     MOVE 717-IDRAPPNR            TO 76A-IDRAPPNR                         
000647     MOVE 717-IDDC                TO 76A-IDDC                             
000648     MOVE 717-TILEVANM            TO 76A-TILEVANM                         
000649     MOVE 717-REEMBHNT            TO 76A-REEMBHNT                         
000650     MOVE 717-RELANDCO            TO 76A-RELANDCO                         
000651     MOVE 717-PRFRAKT             TO 76A-PRFRAKT                          
000652     MOVE 717-PRLEGKST            TO 76A-PRLEGKST                         
000653     MOVE 717-PRFOERS             TO 76A-PRFOERS                          
000654     MOVE WS-ADRESS-1             TO 76A-BEKOPARE-1                       
000655     MOVE WS-ADRESS-2             TO 76A-BEKOPARE-2                       
000656     MOVE WS-ADRESS-3             TO 76A-ADKOPARE-1                       
000657     MOVE WS-ADRESS-4             TO 76A-ADKOPARE-2                       
000658     MOVE DATUM                   TO 76A-DATUM                            
000659     PERFORM S150-SKRIV-W41849-HUVUD                                      
000660     MOVE SPACE                   TO 76A-POST                             
000661     .                                                                    
000662     EJECT                                                                
000663 S90-RADPOST-KNOTA-713 SECTION.                                           
000664     SKIP2                                                                
000665     MOVE '78B'                   TO 78B-IDPTYP                           
000666     MOVE 712-IDDISTR             TO 78B-IDDISTR                          
000667     MOVE 712-IDKUNDNR            TO 78B-IDKUNDNR                         
000668     MOVE 712-IDRAPPNR            TO 78B-IDRAPPNR                         
000669     MOVE 712-IDDC                TO 78B-IDDC                             
000670     MOVE L3-KDFAKTYP             TO 78B-KDFAKTYP                         
000671     MOVE L3-IDFAKT               TO 78B-IDFAKT                           
000672     MOVE L3-TIFAKT               TO 78B-TIFAKT                           
000673     MOVE 713-IDARTNR             TO 78B-IDARTNR                          
000674     MOVE W-BEART                 TO 78B-BEART                            
000675     MOVE 713-KDANMORS            TO 78B-KDANMORS                         
000676     MOVE 713-KVLEVANM            TO 78B-KVLEVANM                         
000677     MOVE 713-KVKREANT            TO 78B-KVKREANT                         
000678     MOVE 713-PRARTBTO            TO 78B-PRARTBTO                         
000679     MOVE 713-IDKNOTNR            TO 78B-IDKNOTNR                         
000680                                                                          
000681     MOVE ZERO                    TO 78B-SUKRENTO-RAD                     
000682     MOVE ZERO                    TO 78B-PRLANDCO-RAD                     
000683     MOVE 713-FLLSBOK             TO 78B-FLLSBOK                          
000684     MOVE 713-IDANALYS            TO 78B-IDANALYS                         
000685     MOVE 713-IDKONTO             TO 78B-IDKONTO                          
000686     MOVE 713-IDKST               TO 78B-IDKST                            
000687                                                                          
000688     PERFORM S153-SKRIV-W41850-RAD                                        
000689     MOVE SPACE                   TO 78B-POST                             
000690     ADD 1                        TO W-RADR-03-713                        
000691                                                                          
000692     MOVE 713-KDANMORS            TO OKOD-KDANMORS                        
000693     CALL W418OKOD USING OKOD-W418OKOD                                    
000694                                                                          
000695     IF OKOD-FL-INTERNUPPACKNING = JA OR                                  
000696        (713-KDANMORS = '00' OR '20' OR '25')                             
000697        PERFORM S120-RADPOST-INTRASTAT                                    
000698     ELSE                                                                 
000699        IF OKOD-FL-RETILL = JA                                            
000700           PERFORM S140-RADPOST-INTRASTAT                                 
000701        END-IF                                                            
000702     END-IF                                                               
000703     .                                                                    
000704     EJECT                                                                
000705 S100-HUVUDPOST-KNOTA-712 SECTION.                                        
000706                                                                          
000707     MOVE '78A'                   TO 78A-IDPTYP                           
000708     MOVE 712-IDDISTR             TO 78A-IDDISTR                          
000709     MOVE 712-IDKUNDNR            TO 78A-IDKUNDNR                         
000710     MOVE 712-IDDC                TO 78A-IDDC                             
000711     MOVE 712-IDKNOTNR            TO 78A-IDKNOTNR                         
000712     MOVE 712-IDRAPPNR            TO 78A-IDRAPPNR                         
000713     MOVE 712-TILEVANM            TO 78A-TILEVANM                         
000714     MOVE 712-TIRETILL            TO 78A-TIRETILL                         
000715     MOVE 712-REEMBHNT            TO 78A-REEMBHNT                         
000716     MOVE 712-RELANDCO            TO 78A-RELANDCO                         
000717     MOVE 712-PRFRAKT             TO 78A-PRFRAKT                          
000718     MOVE 712-PRLEGKST            TO 78A-PRLEGKST                         
000719     MOVE 712-PRFOERS             TO 78A-PRFOERS                          
000720     MOVE WS-ADRESS-1             TO 78A-BEKOPARE-1                       
000721     MOVE WS-ADRESS-2             TO 78A-BEKOPARE-2                       
000722     MOVE WS-ADRESS-3             TO 78A-ADKOPARE-1                       
000723     MOVE WS-ADRESS-4             TO 78A-ADKOPARE-2                       
000724     MOVE W-VALUTA                TO 78A-KDVALISO                         
000725     MOVE W-KURS-LOC              TO 78A-PRKURS-LOC                       
000726     MOVE W-KURS-SDC              TO 78A-PRKURS-SDC                       
000727     MOVE ZERO                    TO 78A-KDVALUT                          
000728     MOVE W-REVALUTA-LOC          TO 78A-REVALUTA-LOC                     
000729     MOVE W-REVALUTA-SDC          TO 78A-REVALUTA-SDC                     
000730     MOVE BET-IDLANDX2            TO 78A-IDLANDX2                         
000731     MOVE BET-IDPARTNR            TO W-WDB1-IDPARTNR                      
000732     MOVE BET-IDFTG               TO W-WDB1-IDFTG                         
000733     MOVE BET-IDVAT               TO 78A-IDVAT                            
000734     MOVE BET-IDLANDX2            TO 78A-IDLANDX2                         
000735                                                                          
000736     MOVE W-FORSAKRAN-DEL-1       TO 78A-FORSAKRAN-1                      
000737     MOVE W-FORSAKRAN-DEL-2       TO 78A-FORSAKRAN-2                      
000738     MOVE DATUM                   TO 78A-DATUM                            
000739     MOVE 712-IDUSER-ADM          TO 78A-IDUSER-ADM                       
000740     MOVE 712-BEANST              TO 78A-BEANST                           
000741     MOVE SPACE                   TO 78A-FLKREPRT                         
000742                                                                          
000743     MOVE ZERO                    TO 78A-SUKRENTO                         
000744     MOVE ZERO                    TO 78A-SUKRENOT                         
000745     MOVE ZERO                    TO 78A-SUKREUTL                         
000746     MOVE ZERO                    TO 78A-PRLANDCO                         
000747     MOVE ZERO                    TO 78A-PREMBHNT                         
000748     MOVE ZERO                    TO 78A-PRMOMS                           
000749                                                                          
000750     MOVE +1           TO GODK-IX                                         
000751     PERFORM UNTIL GODK-IX > MAX-GODK-IX                                  
000752       MOVE 712-ATTESTANSVARIGA(GODK-IX) TO                               
000753                            78A-ATTESTANSVARIGA(GODK-IX)                  
000754       ADD +1          TO GODK-IX                                         
000755     END-PERFORM                                                          
000756                                                                          
000757     PERFORM S152-SKRIV-W41850-HUVUD                                      
000758     MOVE SPACE                   TO 78A-POST                             
000759                                                                          
000760     PERFORM S110-HUVUD-INTRASTAT                                         
000761     PERFORM S130-HUVUD-INTRASTAT                                         
000762     .                                                                    
000763     EJECT                                                                
000764 S110-HUVUD-INTRASTAT             SECTION.                                
000765     SKIP2                                                                
000766     MOVE 'INT'                TO INTRA-IDPTYP                            
000767     MOVE 712-TIKNOTA          TO INTRA-TIFAKT                            
000768     MOVE BET-IDLANDX2         TO INTRA-IDLANDX2                          
000769                                   TEST-IDLANDX2                          
000770                                                                          
000771     IF TEST-IDLANDX2 = SPACE                                             
000772        SEARCH ALL EU-LEV AT END  CONTINUE                                
000773           WHEN EU-IDDISTR (EU-IX) = 712-IDDISTR                          
000774           MOVE EU-IDLANDX2 (EU-IX)                                       
000775                               TO TEST-IDLANDX2                           
000776                                  INTRA-IDLANDX2                          
000777       END-SEARCH                                                         
000778     END-IF                                                               
000779     .                                                                    
000780     EJECT                                                                
000781 S120-RADPOST-INTRASTAT           SECTION.                                
000782     SKIP2                                                                
000783     IF LANDX2-EU-EJ-SE                                                   
000784        MOVE 713-IDARTNR          TO INTRA-IDARTNR                        
000785        COMPUTE INTRA-KVLEVART = (713-KVKREANT *  -1)                     
000786        COMPUTE INTRA-SUFKTBEL    ROUNDED =                               
000787                     (713-KVKREANT * 713-PRARTBTO) * -1                   
000788        MOVE ZERO                 TO INTRA-IDSTATNR                       
000789                                     INTRA-KDINTTYP                       
000790        MOVE SPACE                TO INTRA-IDVAT                          
000791                                     INTRA-KDARTURS                       
000792                                                                          
000793        PERFORM S154-SKRIV-INTRA                                          
000794     END-IF                                                               
000795     .                                                                    
000796     EJECT                                                                
000797 S130-HUVUD-INTRASTAT             SECTION.                                
000798     SKIP2                                                                
000799     MOVE 'ST'                 TO INTRA2-KDSORT                           
000800     MOVE 712-TIKNOTA          TO INTRA2-TIAAMMDD-GAELL                   
000801     MOVE BET-IDLANDX2         TO INTRA2-IDLANDX2                         
000802                                   TEST-IDLANDX2                          
000803                                                                          
000804     IF TEST-IDLANDX2 = SPACE                                             
000805        SEARCH ALL EU-LEV AT END  CONTINUE                                
000806           WHEN EU-IDDISTR (EU-IX) = 712-IDDISTR                          
000807           MOVE EU-IDLANDX2 (EU-IX)                                       
000808                               TO TEST-IDLANDX2                           
000809                                  INTRA2-IDLANDX2                         
000810       END-SEARCH                                                         
000811     END-IF                                                               
000812     .                                                                    
000813     EJECT                                                                
000814 S140-RADPOST-INTRASTAT           SECTION.                                
000815     SKIP2                                                                
000816     IF LANDX2-EU-EJ-SE                                                   
000817        MOVE 713-IDARTNR          TO INTRA2-IDARTNR                       
000818        MOVE 713-KVKREANT         TO INTRA2-KVANTMOT                      
000819        COMPUTE INTRA2-SUARTBES   ROUNDED =                               
000820                      713-KVKREANT * 713-PRARTBTO                         
000821                                                                          
000822        PERFORM S155-SKRIV-INTRA-2                                        
000823     END-IF                                                               
000824     .                                                                    
000825     EJECT                                                                
000826 S150-SKRIV-W41849-HUVUD SECTION.                                         
000827*    DISPLAY '** S150-SKRIV'                                              
000828     MOVE '**S150-SKRIV-W41849-HUVUD' TO FELTEXT-STR                      
000829                                                                          
000830     WRITE UT-W41876A          FROM 76A-POST                              
000831                                                                          
000832     MOVE 76A-IDPTYP           TO POSTSUM-TRANSTYP                        
000833     MOVE 'W41849'             TO POSTSUM-FDNAMN                          
000834     MOVE 'W41840D2'           TO POSTSUM-DDNAMN2                         
000835     CALL POSTSUM USING POSTSUM-PARM                                      
000836     .                                                                    
000837     EJECT                                                                
000838 S151-SKRIV-W41849-RAD SECTION.                                           
000839*    DISPLAY '** S151-SKRIV'                                              
000840     MOVE '**S151-SKRIV-W41849-RAD' TO FELTEXT-STR                        
000841                                                                          
000842     WRITE UT-W41876B          FROM 76B-POST                              
000843                                                                          
000844     MOVE 76B-IDPTYP           TO POSTSUM-TRANSTYP                        
000845     MOVE 'W41849'             TO POSTSUM-FDNAMN                          
000846     MOVE 'W41840D2'           TO POSTSUM-DDNAMN2                         
000847     CALL POSTSUM USING POSTSUM-PARM                                      
000848     .                                                                    
000849     EJECT                                                                
000850 S152-SKRIV-W41850-HUVUD SECTION.                                         
000851*    DISPLAY '** S152-SKRIV'                                              
000852     MOVE '**S152-SKRIV-W41850-HUVUD' TO FELTEXT-STR                      
000853                                                                          
000854     WRITE UT-W41878A          FROM 78A-POST                              
000855                                                                          
000856     MOVE 78A-IDPTYP           TO POSTSUM-TRANSTYP                        
000857     MOVE 'W41850'             TO POSTSUM-FDNAMN                          
000858     MOVE 'W41840D3'           TO POSTSUM-DDNAMN2                         
000859     CALL POSTSUM USING POSTSUM-PARM                                      
000860     .                                                                    
000861     EJECT                                                                
000862 S153-SKRIV-W41850-RAD SECTION.                                           
000863*    DISPLAY '** S153-SKRIV'                                              
000864     MOVE '**S153-SKRIV-W41849-RAD' TO FELTEXT-STR                        
000865                                                                          
000866     WRITE UT-W41878B          FROM 78B-POST                              
000867                                                                          
000868     MOVE 78B-IDPTYP           TO POSTSUM-TRANSTYP                        
000869     MOVE 'W41850'             TO POSTSUM-FDNAMN                          
000870     MOVE 'W41840D3'           TO POSTSUM-DDNAMN2                         
000871     CALL POSTSUM USING POSTSUM-PARM                                      
000872     .                                                                    
000873     EJECT                                                                
000874 S154-SKRIV-INTRA SECTION.                                                
000875*    DISPLAY '** S154-SKRIV'                                              
000876     MOVE '**S154-SKRIV-INTRA' TO FELTEXT-STR                             
000877                                                                          
000878     WRITE INTRA-POST          FROM INTRA-UTAREA                          
000879                                                                          
000880     MOVE 'INT'                TO POSTSUM-TRANSTYP                        
000881     MOVE 'W41854'             TO POSTSUM-FDNAMN                          
000882     MOVE 'W41840D4'           TO POSTSUM-DDNAMN2                         
000883     CALL POSTSUM USING POSTSUM-PARM                                      
000884     .                                                                    
000885     EJECT                                                                
000886 S155-SKRIV-INTRA-2 SECTION.                                              
000887*    DISPLAY '** S155-SKRIV'                                              
000888     MOVE '**S155-SKRIV-INTRA-2' TO FELTEXT-STR                           
000889                                                                          
000890     WRITE INTRA2-POST         FROM INTRA2-UTAREA                         
000891                                                                          
000892     MOVE 'INT'                TO POSTSUM-TRANSTYP                        
000893     MOVE 'W41848'             TO POSTSUM-FDNAMN                          
000894     MOVE 'W41840D5'           TO POSTSUM-DDNAMN2                         
000895     CALL POSTSUM USING POSTSUM-PARM                                      
000896     .                                                                    
000897     EJECT                                                                
000898 IMS-BENA-LAS-ROTSEG      SECTION.                                        
000899                                                                          
000900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
000901          DELIMITED BY SIZE INTO SSA1                                     
000902     MOVE '  GE '               TO GODK-STATUSKODER                       
000903     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-WDD301 SSA1               
000904     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
000905     PERFORM IMS-STATUSKONTROLL                                           
000906     .                                                                    
000907     SKIP2                                                                
000908 IMS-BENA-LAS-TEXTSEG     SECTION.                                        
000909                                                                          
000910     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
000911          DELIMITED BY SIZE INTO SSA1                                     
000912     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-WDD311 SSA1              
000913     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000914     PERFORM IMS-STATUSKONTROLL                                           
000915     .                                                                    
000916     EJECT                                                                
000917 IMS-GET-WLGMTA01 SECTION.                                                
000918     MOVE '*** IMS-GET-WLGMTA01 *** '                                     
000919                              TO FELTEXT-STR                              
000920                                                                          
000921     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
000922                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
000923            DELIMITED BY SIZE INTO SSA1                                   
000924                                                                          
000925     MOVE '  ' TO GODK-STATUSKODER                                        
000926     CALL CBLTDLI USING                                                   
000927           GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1                            
000928     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
000929     PERFORM IMS-STATUSKONTROLL                                           
000930     .                                                                    
000931     EJECT                                                                
000932 IMS-GET-WLGMTA01-UNIK SECTION.                                           
000933     MOVE '*** IMS-GET-WLGMTA01-UNIK *** '                                
000934                              TO FELTEXT-STR                              
000935                                                                          
000936     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
000937            DELIMITED BY SIZE INTO SSA1                                   
000938                                                                          
000939     MOVE '  GE' TO GODK-STATUSKODER                                      
000940     CALL CBLTDLI USING                                                   
000941           GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1                            
000942     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
000943     PERFORM IMS-STATUSKONTROLL                                           
000944     .                                                                    
000945     EJECT                                                                
000946 IMS-GU-WLARTC01 SECTION.                                                 
000947                                                                          
000948     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000949            DELIMITED BY SIZE INTO SSA1                                   
000950                                                                          
000951     MOVE '  GE' TO GODK-STATUSKODER                                      
000952     CALL  CBLTDLI USING                                                  
000953           GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1                            
000954     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000955     PERFORM IMS-STATUSKONTROLL                                           
000956     .                                                                    
000957     EJECT                                                                
000958 IMS-GNP-WLARTC11 SECTION.                                                
000959                                                                          
000960     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
000961            DELIMITED BY SIZE INTO SSA1                                   
000962                                                                          
000963     MOVE '    ' TO GODK-STATUSKODER                                      
000964     CALL  CBLTDLI USING                                                  
000965           GNP ARTC-PCB DLI-IO-AREA-WDK611 SSA1                           
000966     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000967     PERFORM IMS-STATUSKONTROLL                                           
000968     .                                                                    
000969     EJECT                                                                
000970 IMS-GU-WDB101 SECTION.                                                   
000971     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
000972          DELIMITED BY SIZE INTO SSA1                                     
000973     MOVE '  ' TO GODK-STATUSKODER                                        
000980     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
000981     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000982     PERFORM IMS-STATUSKONTROLL                                           
000983     .                                                                    
000984     EJECT                                                                
000985 IMS-GU-WDB601    SECTION.                                                
000986     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000987          DELIMITED BY SIZE INTO SSA1                                     
000988     MOVE '  GE' TO GODK-STATUSKODER                                      
000989     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000990     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000991     PERFORM IMS-STATUSKONTROLL                                           
000992     IF SEGMENT-SAKNAS                                                    
000993         MOVE SPACE TO DCS-KDDC                                           
000994         MOVE 'FEL' TO DCS-KDVALISO                                       
000995     END-IF                                                               
000996     .                                                                    
000997     EJECT                                                                
000998 IMS-STATUSKONTROLL SECTION.                                              
000999                                                                          
001000     SET STATUS-IX          TO 1                                          
001001     SEARCH GODK-STATUS AT END CALL FELLOG                                
001002     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
001003        CONTINUE                                                          
001004     END-SEARCH                                                           
001005     .                                                                    
