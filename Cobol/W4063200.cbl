000001**********************************************************                
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W4063200.                                                
000004 AUTHOR.         MOGREN STINA.                                            
000005 DATE-WRITTEN.   02/03/12.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*    **DDI  SKA IN VID SENARE TILLFÄLLE (030202)                          
000010*                                                                         
000011*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM STARTAS                       
000012*        AV ADD-IT                                                        
000013*                                                                         
000014*        SHIP-IT - INFO TILL ANDRA SYSTEM                                 
000015*        SKEPPNINGS-NUMMER ÄR INDATA TILL PROGRAMMET                      
000016*                                                                         
000017*        VISS INFORMATION SKALL DISTR. OMEDELBART, I NATT-BATCH           
000018*        EL. VID FASTA TIDPUNKTER                                         
000019*        NYA HÄNDELSER SKAPAS I  WDR7                                     
000020*                                                                         
000021*        RSI-TRANSAR SKAPAS I W40630                                      
000022*        SOP-RUTIN W476ST STARTAS     (EDI DIV.TRANSP.)                   
000023*        SOP-RUTIN W476SP STARTAS     (EDI PACK.INFO.)                    
000024*                                                                         
000025*        PROGRAMMET LÄSER      WDE1 (SHIPING)                             
000026*        PROGRAMMET LÄSER      WDE6 (PRODNR-KOLLIREG)                     
000027*        PROGRAMMET LÄSER      WDB2 (KUNDREG)                             
000028*        PROGRAMMET LÄSER      WDB3 (KUNDREG)                             
000029*        PROGRAMMET LÄSER      WDB6 (DCREG)                               
000030*        PROGRAMMET UPPDATERAR WDR7 (TRANSAR TILL BATCH)                  
000031*                                                                         
000032*    INDATA.                                                              
000033*        TRANSAKTION: W40632X                                             
000034*        MID:         W40632I1                                            
000035*                                                                         
000036*    UTTRANS.         W40630X                                             
000037*                                                                         
000038*    E-TR:         RSI-TRANSAR (W40630) BLIR EJ SKAPADE OM                
000039*         25/7-08  SISTA KUNDEN I SKEPPNINGEN EJ ÄR LDC-KUND              
000040*         21/10-10 9902865  NYTT URVAL FÖR EDI FIL TILL TRANSP.           
000041*         20/06-11 10142494 NYTT URVAL FÖR EDI FIL TILL TRANSP.           
000042*         26/06-13 10200987 NYTT URVAL TILL TRANSP. GALLIKER              
000043*         04/11-14 10244338 NYTT URVAL TILL TRANSP. LAGERMAX              
000044*         01/11-18 2614     NYTT URVAL TILL TRANSP. DANX                  
000045*         10/02-19 2615     NYTT URVAL TILL TRANSP. SCHENKER              
000046*         10/02-19 2976     NYTT URVAL TILL TRANSP. DHL                   
000047*         16/10-19 1540086  LDC-1B TILL DK I URVAL TILL TRP.DANX.         
000048*         29/11-19 1569628  DELETE LDC-1B I URVAL TILL POSTNORD-DK        
000049*         10/12-19 1540102  NYTT URVAL TILL TRANSP. BCUBE                 
000050*         09/01-20 1586994  NYTT URVAL TILL TRANSP. BCUBE                 
000051*         25/02-20 1540109  NYTT URVAL TILL TRANSP. NEOVIA                
000052*         20/03-20 1662422  NYTT URVAL TILL TRANSP. DANX.                 
000053*         28/09-20 1735484  NYTT URVAL TILL TRANSP. DANX.                 
000054*                           BORTTAG AV DHL.                               
000055*         15/10-20 1798451  NYTT URVAL TILL TRANSP. DANX.                 
000056*         16/12-20 1895839  NYTT URVAL TILL TRANSP. NEOVIA-BCUBE          
000057*         21/12-20 1906385  NYTT URVAL TILL TRANSP. LAGERMAX.             
000058*         22/05-24 2806690  NYTT URVAL TILL TRANSP. DHL.                  
000059*         22/11-07 3053197  REMOVED THE CALL TO W476S2 ROUTINE            
000060*                           WHICH ORDERS ROUTINE W477S1.                  
000061*                           WHICH ORDERS ROUTINE W477S1.                  
000062*         04/01-23 3151767 NEW DISTR. EDI SCHENKER.                       
000063*         12/01-23 3162155 NEW SELCTION-EDI SCHENKER.                     
000064*         21/02-23 3155814 CLEAN UP OF W476D3 DANZAS FLOW.FILES           
000065*                          CREATED IN W476D3 ARE NOT USED ANYWHERE        
000066*         11/07-23 3421774 NEW SELECTION NEOVIA AND TRUCK&WHEELS          
000067*         13/01-25 4237015 NEW SELECTION DANX                             
000068*         17/02-25 4285328 NEW SELECTION LAGERMAX                         
000069*         25/02-25 4299083 NEW SELECTION DANX                             
000070*         12/06-25 4353779 REMOVE EDI FOR LAGERMAX FOR FC17&50            
000071*                          CHANGE IN  DH- SECTION.                        
000072*         30/10-25 4590609 GEODIS  WILL NO LONGER HANDLE GOODS            
000073*                          FROM CDC.REMOVED CODE RELATED TO               
000074*                          DIST87-GEODIS,DIST87-GEODIS-1,                 
000075*                          DIST87-GEODIS-17.                              
000076*                          SCHENKER WILL HANDLE ALL ORDERCLASS &          
000077*                          MARKETS FOR 3162(AU)                           
000078*                                                                         
000079     SKIP3                                                                
000080 ENVIRONMENT DIVISION.                                                    
000081                                                                          
000082 DATA DIVISION.                                                           
000083     EJECT                                                                
000084 WORKING-STORAGE SECTION.                                                 
000085 77  IDPGM                       PIC X(08)   VALUE 'W4063200'.            
000086                                                                          
000087*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000088 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000089                                                                          
000090 77  JA                          PIC X       VALUE 'J'.                   
000091 77  NEJ                         PIC X       VALUE 'N'.                   
000092                                                                          
000093*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000094 77  POST-SW                     PIC X       VALUE 'J'.                   
000095     88  FIRST-OK                            VALUE 'J'.                   
000096     88  FIRST-FEL                           VALUE 'N'.                   
000097                                                                          
000098 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000099     88  INDATA-OK                           VALUE 'J'.                   
000100     88  INDATA-FEL                          VALUE 'N'.                   
000101                                                                          
000102 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000103     88  NYCKLAR-OK                          VALUE 'J'.                   
000104     88  NYCKLAR-FEL                         VALUE 'N'.                   
000105                                                                          
000106 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000107     88  EGEN-MID                            VALUE '4632'.                
000108     88  GODK-MID                            VALUE '4634' '4632'.         
000109     88  HELP-MID                            VALUE '0551'.                
000110                                                                          
000111 77  FL-W476ST                   PIC X(1)    VALUE 'N'.                   
000112 77  FL-W476SP                   PIC X(1)    VALUE 'N'.                   
000113 77  FL-RSI-TRANS                PIC X(1)    VALUE 'N'.                   
000114                                                                          
000115 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
000116 77  WS-DAGENS-DATUM-POSTN       PIC 9(8)    VALUE ZERO.                  
000117 77  WS-TIDPUNKT                 PIC 9(8)    VALUE ZERO.                  
000118                                                                          
000119 77  WS-FLYG-FRAKT               PIC S9(3)   VALUE +17.                   
000120 77  WS-AIR                      PIC X(3)    VALUE 'AAA'.                 
000121 77  WS-LHS                      PIC X(3)    VALUE 'LHS'.                 
000122                                                                          
000123 01  WS-SEKTION                  PIC X(40)   VALUE SPACES.                
000124                                                                          
000125 01  WS-ADFLGEO-POSTN.                                                    
000126     03 WS-IDDEPOT-POSTN         PIC X(2).                                
000127     03 WS-IDROUTE-POSTN         PIC X(1).                                
000128                                                                          
000129     EJECT                                                                
000130*     ----- VALID IDDC CODES                                              
000131*01  -COPY   WWDC99                                                       
000132     EJECT                                                                
000133                                                                          
000134 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
000135*01  FILLER       -COPY WWDIST03    -RED TEST-IDDISTR.                    
000136                                                                          
000137*01  FILLER       -COPY WWDIST07    -RED TEST-IDDISTR.                    
000138                                                                          
000139*01  FILLER       -COPY WWDIST24    -RED TEST-IDDISTR.                    
000140                                                                          
000141*01  FILLER       -COPY WWDIST31    -RED TEST-IDDISTR.                    
000142                                                                          
000143*01  FILLER       -COPY WWDIST35    -RED TEST-IDDISTR.                    
000144                                                                          
000145*01  FILLER       -COPY WWDIST79    -RED TEST-IDDISTR.                    
000146                                                                          
000147*01  FILLER       -COPY WWDIST87    -RED TEST-IDDISTR.                    
000148                                                                          
000149 01  WS-TEXT                     PIC X(30)   VALUE SPACE.                 
000150                                                                          
000151 01  WS-TIKLOCK                  PIC 9(8)    VALUE ZERO.                  
000152 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000153                                                                          
000154 01  WS-IDCOM              PIC S9(9)   VALUE ZERO COMP-3.                 
000155                                                                          
000156 01  WS-KOLLI-NUM                PIC 9(05) VALUE 0.                       
000157 01  WS-KOLLI REDEFINES WS-KOLLI-NUM.                                     
000158  03  WS-KOLLI-4-5               PIC 9(02).                               
000159  03  WS-KOLLI-1-3               PIC 9(03).                               
000160                                                                          
000161 01  FILLER                      PIC X(16) VALUE 'KOLLI UPPDELN'.         
000162 01  W-KOLLI-KDKOLLI.                                                     
000163     03  W-KDKOLLI-1             PIC X(1).                                
000164     03  W-KDKOLLI-2             PIC X(1).                                
000165     03  W-KDKOLLI-3             PIC X(1).                                
000166     03  FILLER                  PIC X(5).                                
000167                                                                          
000168 01  PARM-AREA                   PIC X(80)   VALUE SPACE.                 
000169 01  FILLER                      REDEFINES PARM-AREA.                     
000170     03  IN-IDSHIPM              PIC 9(7).                                
000171                                                                          
000172 01  WS-IDSHIPM                  PIC S9(7)   VALUE ZERO COMP-3.           
000173                                                                          
000174 01  WS-PRKURS               PIC S9(6)V9(5)  VALUE ZERO COMP-3.           
000175*                                                                         
000176     SKIP2                                                                
000177                                                                          
000178*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000179 01  GENERELLA-SUBPROGRAM.                                                
000180     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000181     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000182     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000183     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000184     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000185     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
000186     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
000187     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000188     EJECT                                                                
000189                                                                          
000190*    --- PARAMETRAR TILL SUBPROGRAM W460DIS1                              
000191*01  -COPY  W460DIS1                                                      
000192                                                                          
000193*    ---                                                                  
000194 01  FILLER                      PIC X(16)   VALUE 'ISO-KODER'.           
000195*01  -COPY  W460LISO                                                      
000196                                                                          
000197     SKIP3                                                                
000198*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000199*                                                                         
000200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000201     SKIP3                                                                
000202*01 -COPY WMSGINIT                                                        
000203     EJECT                                                                
000204*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000205*01  -COPY WMSGAREA                                                       
000206 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000207*01  -COPY WMFSAREA                                                       
000208*                                                                         
000209 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000210     SKIP3                                                                
000211*01  -COPY W40632I1                                                       
000212      EJECT                                                               
000213*    --- AREOR FÖR ANROP FRÅN WZ01                                        
000214 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREAIN'.         
000215*01  -COPY WZ01RECV                                                       
000216                                                                          
000217 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREAUT'.         
000218*01  -COPY WZ01SEND                                                       
000219                                                                          
000220*01  MOD -COPY W40630I1 -PRE 4630-                                        
000221                                                                          
000222*                                                                         
000223*    ---------AREA FÖR SOP-RUTINERS START---                              
000224 01  FILLER                      PIC X(16)   VALUE 'WMSGSOP '.            
000225 01  PROG-TO-PROG-SW.                                                     
000226*    03  -COPY WMSGSOP                                                    
000227*                                                                         
000228 01  WS-BC-PARAMETRAR.                                                    
000229     03  WS-BC.                                                           
000230         05  BC-URV-IDSHIPM      PIC 9(7)    VALUE ZERO.                  
000231                                                                          
000232*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000233*                                                                         
000234 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000235     SKIP3                                                                
000236 01  NYCKLAR-TILL-DLI.                                                    
000237     03  W-IDSHIPM-X.                                                     
000238         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
000239*                                                                         
000240     03  W-WDE111KY-X.                                                    
000241         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000242         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000243*                                                                         
000244     03  W-WDE121KY-X.                                                    
000245         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
000246         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
000247*                                                                         
000248     03  W-KDSEGKY-X.                                                     
000249         05  W-KDSEGKY           PIC X(1)    VALUE SPACE.                 
000250*                                                                         
000251     03  W-IDPURAD-X.                                                     
000252         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
000253*                                                                         
000254     03  W-IDPRODNR-E6-X.                                                 
000255         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
000256*                                                                         
000257     03  W-IDFAKT-X.                                                      
000258         05  W-IDFAKT            PIC S9(09)   VALUE ZERO COMP-3.          
000259*                                                                         
000260     03  W-IDGMT-X.                                                       
000261         05  W-IDDISTR-WDB2      PIC S9(5)  VALUE ZERO COMP-3.            
000262         05  W-IDKUNDNR-WDB2     PIC S9(7)  VALUE ZERO COMP-3.            
000263*                                                                         
000264     03   W-WDB301KY-X.                                                   
000265         05  W-IDDC-B3           PIC X(2)    VALUE SPACE.                 
000266         05  W-IDDISTR-B3        PIC S9(5)   VALUE ZERO COMP-3.           
000267         05  W-IDKUNDNR-B3       PIC S9(7)   VALUE ZERO COMP-3.           
000268*                                                                         
000269     03   W-IDDC-B6-X.                                                    
000270         05  W-IDDC-B6           PIC X(2).                                
000271*                                                                         
000272     03 W-4503-WDGXKEY-X.                                                 
000273       05 W-4503-IDHTYP          PIC X(4)    VALUE '4503'.                
000274       05 W-4503-NYCKEL-VALFRI   PIC X(26)   VALUE LOW-VALUE.             
000275*                                                                         
000276     03 W-4504-WDGXKEY-X.                                                 
000277        05 W-KY4504-IDPRODNR     PIC S9(7)   VALUE ZERO COMP-3.           
000278        05 W-KY4504-IDKOLLI      PIC S9(5)   VALUE ZERO COMP-3.           
000279*                                                                         
000280 EJECT                                                                    
000281     SKIP2                                                                
000282*    --- STATUS-KOD FRÅN IMS                                              
000283 01  STATUS-WS                   PIC XX.                                  
000284     88  SEGMENT-FINNS                       VALUE '  '.                  
000285     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000286     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000287     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000288     SKIP2                                                                
000289 01  GODK-STATUSKODER.                                                    
000290     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000291     SKIP3                                                                
000292 01  SSA1                        PIC X(64).                               
000293 01  SSA2                        PIC X(64).                               
000294 01  SSA3                        PIC X(64).                               
000295     EJECT                                                                
000296*    --- IMS FUNKTIONSKODER                                               
000297*01  -COPY W0003                                                          
000298     EJECT                                                                
000299*    ---  DLI INPUT-OUTPUT AREA                                           
000300                                                                          
000301 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE101'.          
000302 01  DLI-IO-WDE101.                                                       
000303*    03  -COPY WDE101                                                     
000304     EJECT                                                                
000305 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE111'.          
000306 01  DLI-IO-WDE111.                                                       
000307*    03  -COPY WDE111                                                     
000308     EJECT                                                                
000309 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDE121'.          
000310 01  DLI-IO-WDE121.                                                       
000311*    03  -COPY WDE121                                                     
000312     EJECT                                                                
000313 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDR701'.          
000314 01  DLI-IO-WDR701.                                                       
000315*    03  -COPY WDR701                                                     
000316 01  FILLER                     PIC X(16) VALUE 'DLI-IO-E601'.            
000317 01  DLI-IO-WDE601.                                                       
000318*    03  -COPY WDE601                                                     
000319     EJECT                                                                
000320 01  FILLER                     PIC X(16) VALUE 'WDB201-AREA'.            
000321 01  DLI-IO-WDB201.                                                       
000322     03  WLGMTA01.                                                        
000323*        05  -COPY WDB201                                                 
000324     EJECT                                                                
000325 01  FILLER                     PIC X(16) VALUE 'WDB301-AREA'.            
000326 01  DLI-IO-WDB301.                                                       
000327     03  -COPY WDB301                                                     
000328     EJECT                                                                
000329 01  FILLER                     PIC X(16) VALUE 'WDB601-AREA'.            
000330 01  DLI-IO-AREA-B601.                                                    
000331*    03  -COPY WDB601                                                     
000332     EJECT                                                                
000333 01  FILLER                     PIC X(16) VALUE 'DLI-IO-4504'.            
000334 01  DLI-IO-4504.                                                         
000335*    03  -COPY WDGX4504                                                   
000336     EJECT                                                                
000337 LINKAGE SECTION.                                                         
000338                                                                          
000339 01  IO-PCB                      PIC X.                                   
000340                                                                          
000341*01  -COPY W0009  -PRE ALT-                                               
000342     EJECT                                                                
000343*01  -COPY W0009  -PRE SH30-                                              
000344     EJECT                                                                
000345*01  -COPY W0008  -PRE WDE1-                                              
000346     05  FILLER                  PIC X.                                   
000347                                                                          
000348*01  -COPY W0008  -PRE WDR7-                                              
000349     05  FILLER                  PIC X.                                   
000350                                                                          
000351*01  -COPY W0008  -PRE WDE6-                                              
000352     05  FILLER                  PIC X.                                   
000353                                                                          
000354*01  -COPY W0008  -PRE WDB2-                                              
000355     05  FILLER                  PIC X.                                   
000356                                                                          
000357*01  -COPY W0008  -PRE WDB3-                                              
000358     05  FILLER                  PIC X.                                   
000359                                                                          
000360*01  -COPY W0008  -PRE WDB6-                                              
000361     05  FILLER                  PIC X.                                   
000362                                                                          
000363*01  -COPY W0008  -PRE 4503-                                              
000364     05  FILLER                  PIC X.                                   
000365     EJECT                                                                
000366 PROCEDURE DIVISION       USING   IO-PCB                                  
000367                                 ALT-PCB                                  
000368                                SH30-PCB                                  
000369                                WDE1-PCB                                  
000370                                WDR7-PCB                                  
000371                                WDE6-PCB                                  
000372                                WDB2-PCB                                  
000373                                WDB3-PCB                                  
000374                                WDB6-PCB                                  
000375                                4503-PCB.                                 
000376 MAIN SECTION.                                                            
000377     ENTRY 'DLITCBL'      USING   IO-PCB                                  
000378                                 ALT-PCB                                  
000379                                SH30-PCB                                  
000380                                WDE1-PCB                                  
000381                                WDR7-PCB                                  
000382                                WDE6-PCB                                  
000383                                WDB2-PCB                                  
000384                                WDB3-PCB                                  
000385                                WDB6-PCB                                  
000386                                4503-PCB.                                 
000387                                                                          
000388     PERFORM A-INIT                                                       
000389     PERFORM B-KOLLA-NYCKLAR                                              
000390     IF NYCKLAR-OK                                                        
000391         MOVE WS-IDSHIPM             TO W-IDSHIPM                         
000392         PERFORM IMS-GU-WDE101                                            
000393         MOVE SHIP-IDDC              TO W-IDDC-B6                         
000394         PERFORM IMS-GU-WDB601                                            
000395         PERFORM IMS-GNP-WDE111                                           
000396         MOVE SGMT-IDDISTR           TO W-IDDISTR                         
000397         MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR                        
000398         PERFORM UNTIL SEGMENT-SAKNAS                                     
000399           PERFORM IMS-GNP-WDE121                                         
000400           MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                        
000401           MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                         
000402           PERFORM UNTIL SEGMENT-SAKNAS                                   
000403                                                                          
000404             PERFORM D-TRANSPORT-HUV-INFO                                 
000405             PERFORM IMS-GNP-WDE121                                       
000406             MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                        
000407             MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                         
000408           END-PERFORM                                                    
000409           PERFORM IMS-GNP-WDE111                                         
000410           MOVE SGMT-IDDISTR         TO W-IDDISTR                         
000411           MOVE SGMT-IDKUNDNR        TO W-IDKUNDNR                        
000412         END-PERFORM                                                      
000413     END-IF                                                               
000414     PERFORM Z-FINIT                                                      
000415                                                                          
000416     MOVE ZERO TO RETURN-CODE                                             
000417     GOBACK                                                               
000418     .                                                                    
000419     EJECT                                                                
000420 A-INIT SECTION.                                                          
000421                                                                          
000422     PERFORM S11-RECV-OPEN                                                
000423     PERFORM S12-RECV-MESSAGE                                             
000424     PERFORM S13-RECV-CLOSE                                               
000425     ACCEPT DAGENS-DATUM            FROM DATE                             
000426     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000427                                 TO WS-DAGENS-DATUM-POSTN                 
000428     ACCEPT WS-TIDPUNKT             FROM TIME                             
000429     ACCEPT WS-TIKLOCK              FROM TIME                             
000430     ACCEPT FIL-TIKLOCK             FROM TIME                             
000431     MOVE ZERO                   TO GMT-IDDISTR                           
000432                                    GMT-IDKUNDNR                          
000433                                    FIL-IDSEKVNR                          
000434     .                                                                    
000435     EJECT                                                                
000436 B-KOLLA-NYCKLAR SECTION.                                                 
000437                                                                          
000438     MOVE MID-IDSHIPM            TO IN-IDSHIPM                            
000439     MOVE IN-IDSHIPM             TO WS-IDSHIPM                            
000440                                    BC-URV-IDSHIPM                        
000441                                                                          
000442     MOVE JA TO NYCKLAR-SW                                                
000443     IF WS-IDSHIPM = ZERO                                                 
000444       MOVE NEJ TO NYCKLAR-SW                                             
000445     END-IF                                                               
000446     IF NYCKLAR-FEL                                                       
000447       STRING 'SKEPPNINGS-NUMMER SAKNAS '                                 
000448            DELIMITED BY SIZE INTO FELTEXT                                
000449       CALL FELLOG                                                        
000450     END-IF                                                               
000451     .                                                                    
000452     EJECT                                                                
000453                                                                          
000454 D-TRANSPORT-HUV-INFO SECTION.                                            
000455     MOVE 'D-TRANSPORT-HUV-INFO'  TO WS-SEKTION                           
000456                                                                          
000457     MOVE SGMT-IDDISTR           TO W-IDDISTR-B3                          
000458     MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR-B3                         
000459     MOVE SHIP-IDDC              TO W-IDDC-B3                             
000460     PERFORM IMS-GU-WDB301                                                
000461     IF SEGMENT-SAKNAS                                                    
000462        MOVE ZERO                TO DC-KDTULLVE                           
000463     END-IF                                                               
000464                                                                          
000465     IF FIRST-OK                                                          
000466       PERFORM DB-SKAPA-EXP-EMP-PROF                                      
000467                                                                          
000468       PERFORM DC-SKAPA-EMB-VIPS                                          
000469     END-IF                                                               
000470                                                                          
000471     IF FIRST-OK                                                          
000472                                                                          
000473       PERFORM DG-TRANSPORT-INFO-VR                                       
000474                                                                          
000475       PERFORM DI-SKAPA-BROKER-INFO                                       
000476     END-IF                                                               
000477                                                                          
000478     PERFORM DH-SKAPA-DIV-TRANSP-INFO                                     
000479                                                                          
000480     PERFORM DK-SKAPA-RSITRANS                                            
000481                                                                          
000482     MOVE NEJ          TO POST-SW                                         
000483     .                                                                    
000484     EJECT                                                                
000485 DB-SKAPA-EXP-EMP-PROF  SECTION.                                          
000486     MOVE 'DB-SKAPA-EXP-EMP-PROF' TO WS-SEKTION                           
000487                                                                          
000488     MOVE SHIP-IDDC               TO WS-IDDC                              
000489                                                                          
000490     MOVE 'W4063200'              TO FIL-IDPGM                            
000491     MOVE DAGENS-DATUM            TO FIL-TIREGDAT                         
000492     ADD +1                       TO FIL-IDSEKVNR                         
000493     MOVE 'W476'                  TO FIL-CT-IDSYSTEM                      
000494     MOVE 'EXP'                   TO FIL-CT-IDPTYP                        
000495     MOVE SPACE                   TO FIL-CT-IDVTYP                        
000496     MOVE SPACE                   TO FIL-WDR701-DATA                      
000497     MOVE WS-BC-PARAMETRAR        TO FIL-WDR701-DATA                      
000498     PERFORM IMS-ISRT-WDR701                                              
000499     PERFORM S12-WDR7-FINNS                                               
000500     .                                                                    
000501     EJECT                                                                
000502 DC-SKAPA-EMB-VIPS SECTION.                                               
000503     MOVE 'DC-SKAPA-EMB-VIPS'    TO WS-SEKTION                            
000504                                                                          
000505     MOVE 'W4063200'             TO FIL-IDPGM                             
000506     MOVE DAGENS-DATUM           TO FIL-TIREGDAT                          
000507     ADD +1                      TO FIL-IDSEKVNR                          
000508     MOVE 'W476'                 TO FIL-CT-IDSYSTEM                       
000509     MOVE 'EMB'                  TO FIL-CT-IDPTYP                         
000510     MOVE SPACE                  TO FIL-CT-IDVTYP                         
000511     MOVE SPACE                  TO FIL-WDR701-DATA                       
000512     MOVE WS-BC-PARAMETRAR       TO FIL-WDR701-DATA                       
000513     PERFORM IMS-ISRT-WDR701                                              
000514     PERFORM S12-WDR7-FINNS                                               
000515     .                                                                    
000516     EJECT                                                                
000517 DG-TRANSPORT-INFO-VR  SECTION.                                           
000518     MOVE 'DG-TRANSPORT-INFO-VR'   TO WS-SEKTION                          
000519                                                                          
000520     MOVE SKOLLI-IDDISTR            TO W-IDDISTR-WDB2                     
000521     MOVE SKOLLI-IDKUNDNR           TO W-IDKUNDNR-WDB2                    
000522     IF SKOLLI-IDDISTR  NOT = GMT-IDDISTR  OR                             
000523        SKOLLI-IDKUNDNR NOT = GMT-IDKUNDNR                                
000524        PERFORM IMS-GU-WDB201                                             
000525     END-IF                                                               
000526     MOVE SHIP-IDDC                 TO WS-IDDC                            
000527     IF  (SKOLLI-KDFAKTYP  = 'R' OR 'G' OR 'K' OR 'N')                    
000528*          DC-KDTULLVE = ZERO                                             
000529*      TRANS FÖR INFO TILL VR                                             
000530       IF GMT-FLVR     = JA                                               
000540           MOVE SGMT-IDDISTR        TO DIS1-IDDISTR                       
000550                                       TEST-IDDISTR                       
000560                                                                          
000570           CALL W460DIS1 USING DIS1-W460DIS1                              
000580                                                                          
000590           IF DIS1-IDLANDX2 = ISO-SVERIGE  OR                             
000600              ISO-DANMARK OR ISO-NORGE     OR                             
000601              ISO-PORTUGAL OR                                             
000602              ISO-HOLLAND  OR                                             
000603              DIST24-NORGE                                                
000604             CONTINUE                                                     
000605           ELSE                                                           
000606                                                                          
000607             MOVE 'W4063200'              TO FIL-IDPGM                    
000608             MOVE DAGENS-DATUM            TO FIL-TIREGDAT                 
000609             ADD +1                       TO FIL-IDSEKVNR                 
000610             MOVE 'W476'                  TO FIL-CT-IDSYSTEM              
000611             MOVE 'VR '                   TO FIL-CT-IDPTYP                
000612             MOVE SPACE                   TO FIL-CT-IDVTYP                
000613             MOVE SPACE                   TO FIL-WDR701-DATA              
000614             MOVE WS-BC-PARAMETRAR        TO FIL-WDR701-DATA              
000615             PERFORM IMS-ISRT-WDR701                                      
000616             PERFORM S12-WDR7-FINNS                                       
000617           END-IF                                                         
000618       END-IF                                                             
000619     END-IF                                                               
000620                                                                          
000621     .                                                                    
000622     EJECT                                                                
000623 DH-SKAPA-DIV-TRANSP-INFO SECTION.                                        
000624     MOVE 'DH-SKAPA-TRANSP-INFO' TO WS-SEKTION                            
000625                                                                          
000626     IF SKOLLI-IDPRODNR NOT = W-IDPRODNR-E6                               
000627       MOVE SKOLLI-IDPRODNR            TO W-IDPRODNR-E6                   
000628       PERFORM IMS-GU-WDE601                                              
000629     END-IF                                                               
000630                                                                          
000631     MOVE SKOLLI-IDDISTR          TO TEST-IDDISTR                         
000632     MOVE SHIP-IDDC               TO WS-IDDC                              
000633                                                                          
000634     IF SDC-NL AND DIST87-NIGHT-PLUS                                      
000635                                                                          
000636       IF (SKOLLI-KDORDKL = +0 OR +1 OR +3 OR +4) AND                     
000637          (SKOLLI-KDFRAKT = +1 OR +13 OR +14 OR +15 OR +29)               
000638                                                                          
000639         MOVE JA                   TO FL-W476ST                           
000640       END-IF                                                             
000641     END-IF                                                               
000642                                                                          
000643     IF (CDC-SE AND DIST87-LAGERMAX) OR                                   
000644        (DDC-SE AND DIST87-LAGERMAX AND VORD-KDVIA = '01') OR             
000645        (SDC-NL AND DIST87-LAGERMAX) OR                                   
000646        (SDC-AT AND DIST87-LAGERMAX)                                      
000647                                                                          
000648       IF (SKOLLI-KDFRAKT = +17 OR                                        
000649           SKOLLI-KDFRAKT = +50)                                          
000650          CONTINUE                                                        
000651       ELSE                                                               
000652          MOVE JA               TO FL-W476ST                              
000653       END-IF                                                             
000654     END-IF                                                               
000655                                                                          
000656     IF (LDC-NL-3R AND DIST87-LAGERMAX)                                   
000657       IF (SKOLLI-KDFRAKT = +17 OR                                        
000658           SKOLLI-KDFRAKT = +50)                                          
000659          CONTINUE                                                        
000660       ELSE                                                               
000661          IF (SKOLLI-KDORDKL = +0)  AND                                   
000662             (SKOLLI-KDFRAKT = +31)                                       
000663                                                                          
000664            MOVE JA                 TO FL-W476ST                          
000665          END-IF                                                          
000666       END-IF                                                             
000667     END-IF                                                               
000668                                                                          
000669     IF (CDC-SE AND DIST87-LAGERMAX-LYNK) OR                              
000670        (DDC-SE AND DIST87-LAGERMAX-LYNK AND VORD-KDVIA = '01') OR        
000671        (SDC-NL AND DIST87-LAGERMAX-LYNK) OR                              
000672        (SDC-AT AND DIST87-LAGERMAX-LYNK)                                 
000673                                                                          
000674       IF ((SKOLLI-KDORDKL = +0)  AND                                     
000675          (SKOLLI-KDFRAKT = +17))                                         
000676          OR                                                              
000677          (SKOLLI-KDFRAKT = +17 OR                                        
000678           SKOLLI-KDFRAKT = +50)                                          
000679         CONTINUE                                                         
000680       ELSE                                                               
000681         MOVE JA                 TO FL-W476ST                             
000682       END-IF                                                             
000683     END-IF                                                               
000684                                                                          
000685     IF (DDC-FR AND DIST87-LAGERMAX)                                      
000686                                                                          
000687      MOVE JA                     TO FL-W476ST                            
000688     END-IF                                                               
000690                                                                          
000691     IF (CDC-SE AND DIST87-TRUCKWHEEL) OR                                 
000692        (SDC-NL AND DIST87-TRUCKWHEEL) OR                                 
000693        (SDC-ES AND DIST87-TRUCKWHEEL) OR                                 
000694        (SDC-AT AND DIST87-TRUCKWHEEL) OR                                 
000695        (SDC-IT AND DIST87-TRUCKWHEEL) OR                                 
000696        (DDC-SE AND DIST87-TRUCKWHEEL AND VORD-KDVIA = '01')              
000697                                       OR                                 
000698        (DIST87-TRUCKWHEEL AND (LDC-SE OR LDC-GB OR LDC-DE OR             
000699                                LDC-BE OR LDC-CH OR LDC-FI OR             
000700                                LDC-IT OR LDC-NL OR LDC-NO OR             
000701                                LDC-PL))                                  
000702                                                                          
000703       IF SKOLLI-KDFRAKT NOT = +17                                        
000704                                                                          
000705         MOVE JA                   TO FL-W476ST                           
000706         MOVE JA                   TO FL-W476SP                           
000707       END-IF                                                             
000708     END-IF                                                               
000709                                                                          
000710     IF (LDC-FR-3P AND DIST87-TRUCKWHEEL)                                 
000711                                                                          
000712       MOVE JA                     TO FL-W476ST                           
000713       MOVE JA                     TO FL-W476SP                           
000714     END-IF                                                               
000715                                                                          
000716     IF (DDC-FR  AND DIST87-TRUCKWHEEL)                                   
000717                                                                          
000718       MOVE JA                     TO FL-W476ST                           
000719       MOVE JA                     TO FL-W476SP                           
000720     END-IF                                                               
000721                                                                          
000722     IF (CDC-SE AND DIST87-GALLIKER)                                      
000723                                                                          
000724       IF (SKOLLI-KDORDKL = +1 OR +3 OR +4) AND                           
000725          (SKOLLI-KDFRAKT = +13 OR +31)                                   
000726                                                                          
000727         MOVE JA                   TO FL-W476ST                           
000728       END-IF                                                             
000729     END-IF                                                               
000730                                                                          
000731     IF (DDC-SE AND DIST87-GALLIKER AND VORD-KDVIA = '01')                
000732                                                                          
000733       IF (SKOLLI-KDORDKL = +1 OR +3 OR +4)                               
000734                                                                          
000735         MOVE JA                   TO FL-W476ST                           
000736       END-IF                                                             
000737     END-IF                                                               
000738                                                                          
000739     IF (SDC-NL AND DIST87-GALLIKER)                                      
000740                                                                          
000741       IF (SKOLLI-KDORDKL = +1 OR +3 OR +4) AND                           
000742          (SKOLLI-KDFRAKT = +31)                                          
000743                                                                          
000744         MOVE JA                   TO FL-W476ST                           
000745       END-IF                                                             
000746     END-IF                                                               
000747                                                                          
000748     IF (LDC-CH-3H AND DIST87-GALLIKER)                                   
000749                                                                          
000750       IF (SKOLLI-KDORDKL = +0 OR +1 OR +3 OR +4) AND                     
000751          (SKOLLI-KDFRAKT = +13)                                          
000752                                                                          
000753         MOVE JA                   TO FL-W476ST                           
000754       END-IF                                                             
000755     END-IF                                                               
000756                                                                          
000757     IF (CDC-SE    AND DIST87-DANX) OR                                    
000758        (LDC-FI-3O AND DIST87-DANX) OR                                    
000759        (DDC-SE    AND DIST87-DANX  AND VORD-KDVIA = '01')                
000760                                                                          
000761       IF (SKOLLI-KDORDKL = +1 OR +2 OR +3 OR +4)                         
000762                                                                          
000763         MOVE JA                   TO FL-W476ST                           
000770       END-IF                                                             
000771     END-IF                                                               
000772                                                                          
000773     IF LDC-SE-1C AND DIST87-DANX                                         
000774                                                                          
000775       IF (SKOLLI-KDORDKL = +0)                                           
000776                                                                          
000777         MOVE JA                   TO FL-W476ST                           
000778       END-IF                                                             
000779     END-IF                                                               
000780                                                                          
000781     IF (CDC-SE    AND DIST87-DANX-LYNK) OR                               
000782        (LDC-FI-3O AND DIST87-DANX-LYNK) OR                               
000783        (DDC-SE    AND DIST87-DANX-LYNK  AND VORD-KDVIA = '01')           
000784                                                                          
000785       IF (SKOLLI-KDFRAKT = +31)                                          
000786                                                                          
000787         MOVE JA                   TO FL-W476ST                           
000788       END-IF                                                             
000789     END-IF                                                               
000790                                                                          
000800     IF (CDC-SE    AND DIST87-DANX-DK) OR                                 
000810        (LDC-SE-1B AND DIST87-DANX-DK) OR                                 
000811        (DDC-SE    AND DIST87-DANX-DK  AND VORD-KDVIA = '01')             
000812                                                                          
000813       IF (SKOLLI-KDFRAKT = +63)                                          
000814                                                                          
000815         MOVE JA                   TO FL-W476ST                           
000816       END-IF                                                             
000817     END-IF                                                               
000818                                                                          
000819     IF (CDC-SE    AND DIST87-DANX-DK) OR                                 
000820        (DDC-SE    AND DIST87-DANX-DK  AND VORD-KDVIA = '01')             
000821                                                                          
000822       IF (SKOLLI-KDFRAKT = +35)                                          
000823                                                                          
000824         MOVE JA                   TO FL-W476ST                           
000825       END-IF                                                             
000826     END-IF                                                               
000827                                                                          
000828     IF (CDC-SE    AND DIST87-DANX-SE) OR                                 
000829        (LDC-SE-1C AND DIST87-DANX-SE) OR                                 
000830        (DDC-SE    AND DIST87-DANX-SE  AND VORD-KDVIA = '01')             
000831                                                                          
000832       IF (SKOLLI-KDFRAKT = +87)                                          
000833                                                                          
000834         MOVE JA                   TO FL-W476ST                           
000835       END-IF                                                             
000836     END-IF                                                               
000837*                                                                         
000838     IF (CDC-SE    AND DIST87-DANX-SE) OR                                 
000839        (LDC-SE-1C AND DIST87-DANX-SE)                                    
000840                                                                          
000850       IF (SKOLLI-KDFRAKT = +5)                                           
000860                                                                          
000870         MOVE JA                   TO FL-W476ST                           
000880       END-IF                                                             
000890     END-IF                                                               
000900                                                                          
000901     IF DDC-SE AND DIST87-DANX-SE AND VORD-KDVIA = '01'                   
000902                                                                          
000903       IF SKOLLI-KDFRAKT = +5                                             
000904                                                                          
000905         IF VORD-IDLEVNR = 'BQ8VA' OR                                     
000906                           'CWZYA' OR                                     
000907                           'AEL4V'                                        
000908           MOVE JA                 TO FL-W476ST                           
000909         END-IF                                                           
000910       END-IF                                                             
000911     END-IF                                                               
000912                                                                          
000913     IF (CDC-SE    AND DIST87-DANX-POLEN) OR                              
000914        (LDC-PL-3S AND DIST87-DANX-POLEN) OR                              
000915        (SDC-NL    AND DIST87-DANX-POLEN) OR                              
000916        (DDC-SE    AND DIST87-DANX-POLEN AND VORD-KDVIA = '01')           
000917                                                                          
000918       MOVE JA                     TO FL-W476ST                           
000919     END-IF                                                               
000920                                                                          
000930     IF (CDC-SE    AND DIST87-SCHENKER) OR                                
000940        (DDC-SE    AND DIST87-SCHENKER  AND VORD-KDVIA = '01')            
000950                                                                          
000960         MOVE JA                   TO FL-W476ST                           
000970     END-IF                                                               
000971                                                                          
000972     IF (CDC-SE    AND DIST87-BCUBE) OR                                   
000973        (DDC-SE    AND DIST87-BCUBE  AND VORD-KDVIA = '01')               
000974                                                                          
000975       MOVE JA                     TO FL-W476ST                           
000976       MOVE JA                     TO FL-W476SP                           
000977     END-IF                                                               
000978                                                                          
000979     IF (SDC-IT    AND DIST87-BCUBE)                                      
000980                                                                          
000981       IF (SKOLLI-KDFRAKT = +30 OR +31)                                   
000982                                                                          
000983         MOVE JA                   TO FL-W476ST                           
000984         MOVE JA                   TO FL-W476SP                           
000985       END-IF                                                             
000986     END-IF                                                               
000987                                                                          
000988     IF (LDC-IT-3D AND DIST87-BCUBE) OR                                   
000989        (LDC-IT-3F AND DIST87-BCUBE)                                      
000990                                                                          
000991       MOVE JA                     TO FL-W476ST                           
000992       MOVE JA                     TO FL-W476SP                           
000993     END-IF                                                               
000994                                                                          
000995     IF (SDC-NL AND DIST87-BCUBE)                                         
000996                                                                          
000997       IF (SKOLLI-KDORDKL = +1 OR +2 OR +3 OR +4)                         
000998                                                                          
000999         MOVE JA                   TO FL-W476ST                           
001000         MOVE JA                   TO FL-W476SP                           
001001       END-IF                                                             
001002     END-IF                                                               
001003                                                                          
001004     IF (SDC-NL AND DIST87-BCUBE)                                         
001005                                                                          
001006       IF (SKOLLI-KDORDKL = +0) AND                                       
001007          (SKOLLI-KDFRAKT = +31)                                          
001008                                                                          
001009         MOVE JA                   TO FL-W476ST                           
001010       END-IF                                                             
001011     END-IF                                                               
001012                                                                          
001013     IF (CDC-SE    AND DIST87-BCUBE-PLUS) OR                              
001014        (DDC-SE    AND DIST87-BCUBE-PLUS  AND VORD-KDVIA = '01')          
001015                                                                          
001016       MOVE JA                     TO FL-W476ST                           
001017       MOVE JA                     TO FL-W476SP                           
001018     END-IF                                                               
001019                                                                          
001020     IF (LDC-IT-3F AND DIST87-BCUBE-PLUS)                                 
001021                                                                          
001022       MOVE JA                     TO FL-W476ST                           
001023       MOVE JA                     TO FL-W476SP                           
001024     END-IF                                                               
001025                                                                          
001026     IF (SDC-NL AND DIST87-BCUBE-PLUS)                                    
001027                                                                          
001028       IF (SKOLLI-KDORDKL = +0) AND                                       
001029          (SKOLLI-KDFRAKT = +31)                                          
001030                                                                          
001031         MOVE JA                   TO FL-W476ST                           
001032       END-IF                                                             
001033     END-IF                                                               
001034                                                                          
001035     IF (DDC-FR   AND DIST87-BCUBE) OR                                    
001036        (DDC-FR   AND DIST87-BCUBE-PLUS)                                  
001037                                                                          
001038       MOVE JA                     TO FL-W476ST                           
001039       MOVE JA                     TO FL-W476SP                           
001040     END-IF                                                               
001041                                                                          
001042     IF (SDC-ES    AND DIST87-NEOVIA-ES) OR                               
001043        (SDC-ES    AND DIST87-NEOVIA-AFRIKA)                              
001044                                                                          
001045       MOVE JA                     TO FL-W476ST                           
001046       MOVE JA                     TO FL-W476SP                           
001047     END-IF                                                               
001048                                                                          
001049     IF (CDC-SE    AND DIST87-NEOVIA-ES) OR                               
001050        (SDC-NL    AND DIST87-NEOVIA-ES) OR                               
001051        (SDC-AT    AND DIST87-NEOVIA-ES) OR                               
001052        (DDC-SE    AND DIST87-NEOVIA-ES AND VORD-KDVIA = '01')            
001053                                         OR                               
001054        (DIST87-NEOVIA-ES AND (LDC-SE OR LDC-GB OR LDC-DE OR              
001055                               LDC-BE OR LDC-CH OR LDC-FI OR              
001056                               LDC-FR OR LDC-IT OR LDC-NL OR              
001057                               LDC-NO OR LDC-PL))                         
001058                                                                          
001059       IF SKOLLI-KDFRAKT NOT = +17                                        
001060                                                                          
001061         MOVE JA                   TO FL-W476ST                           
001062         MOVE JA                   TO FL-W476SP                           
001063       END-IF                                                             
001064     END-IF                                                               
001065                                                                          
001066     IF (DDC-FR AND DIST87-NEOVIA-ES)                                     
001067                                                                          
001068       MOVE JA                     TO FL-W476ST                           
001069       MOVE JA                     TO FL-W476SP                           
001070     END-IF                                                               
001071                                                                          
001072*    IF (CDC-SE    AND DIST87-NEOVIA-AFRIKA) OR                           
001073*       (DDC-SE    AND DIST87-NEOVIA-AFRIKA AND VORD-KDVIA = '01')        
001074*                                                                         
001075*      IF (SKOLLI-KDORDKL = +1 OR +4)  AND                                
001076*         (SKOLLI-KDFRAKT = +31 OR +41 OR +42 OR +43)                     
001077*                                                                         
001078*        MOVE JA                   TO FL-W476ST                           
001079*      END-IF                                                             
001080*    END-IF                                                               
001081                                                                          
001082     IF (LDC-SE-1A AND DIST87-DHL)                                        
001083                                                                          
001084       IF (SKOLLI-KDFRAKT = +46)                                          
001085                                                                          
001086         MOVE JA                   TO FL-W476ST                           
001087       END-IF                                                             
001088     END-IF                                                               
001089                                                                          
001090     IF (LDC-SE-1B AND DIST87-DHL)                                        
001091                                                                          
001092       IF (SKOLLI-KDFRAKT = +63)                                          
001093                                                                          
001094         MOVE JA                   TO FL-W476ST                           
001095       END-IF                                                             
001096     END-IF                                                               
001097                                                                          
001098     IF (LDC-SE-1D AND DIST87-DHL)                                        
001099                                                                          
001100       IF (SKOLLI-KDFRAKT = +52)                                          
001101                                                                          
001102         MOVE JA                   TO FL-W476ST                           
001103       END-IF                                                             
001104     END-IF                                                               
001105                                                                          
001106     IF (LDC-SE-1E AND DIST87-DHL)                                        
001107                                                                          
001108       IF (SKOLLI-KDFRAKT = +68)                                          
001109                                                                          
001110         MOVE JA                   TO FL-W476ST                           
001111       END-IF                                                             
001112     END-IF                                                               
001113                                                                          
001114     IF (CDC-SE    AND DIST87-DHL-NO) OR                                  
001115        (LDC-NO-3J AND DIST87-DHL-NO) OR                                  
001116        (DDC-SE    AND DIST87-DHL-NO  AND VORD-KDVIA = '01')              
001117                                                                          
001118       IF (SKOLLI-KDFRAKT NOT = +31)                                      
001119                                                                          
001120         MOVE JA                   TO FL-W476ST                           
001121       END-IF                                                             
001122     END-IF                                                               
001123     .                                                                    
001124     EJECT                                                                
001125 DI-SKAPA-BROKER-INFO  SECTION.                                           
001126     MOVE 'DI-SKAPA-BROKER-INFO'   TO WS-SEKTION                          
001127                                                                          
001128     MOVE SKOLLI-IDDISTR          TO TEST-IDDISTR                         
001129     MOVE SHIP-IDDC               TO WS-IDDC                              
001130     IF SHIP-IDLBBET(1:4) NOT = 'SOFT' AND                                
001131        SKOLLI-KDFAKTYP NOT = 'P'                                         
001132       IF ((CDC-SE OR                                                     
001133            DDC-SE OR DDC-NO OR DDC-BE OR DDC-DE) AND                     
001134            DIST07-NA-CUSTOMERS)                  OR                      
001135          DIST35-REFILL-NA                        OR                      
001136          DIST35-REFILL-NA-JAP                    OR                      
001137          DIST35-NDCCN-NDCUS-REFILL                                       
001138         MOVE 'W4063200'             TO FIL-IDPGM                         
001139         MOVE DAGENS-DATUM           TO FIL-TIREGDAT                      
001140         ADD +1                      TO FIL-IDSEKVNR                      
001141         MOVE 'W476'                 TO FIL-CT-IDSYSTEM                   
001142         MOVE 'BRO'                  TO FIL-CT-IDPTYP                     
001143         MOVE SPACE                  TO FIL-CT-IDVTYP                     
001144         MOVE SPACE                  TO FIL-WDR701-DATA                   
001145         MOVE WS-BC-PARAMETRAR       TO FIL-WDR701-DATA                   
001146                                                                          
001147         PERFORM IMS-ISRT-WDR701                                          
001148         PERFORM S12-WDR7-FINNS                                           
001149       END-IF                                                             
001150     END-IF                                                               
001151                                                                          
001152     MOVE SKOLLI-IDDISTR          TO DIS1-IDDISTR                         
001153     CALL W460DIS1 USING DIS1-W460DIS1                                    
001154     IF SHIP-IDLBBET(1:4) NOT = 'SOFT'                                    
001155       IF ((CDC-SE OR GOOD-DDC) AND                                       
001156            DIS1-IDLANDX2 = ISO-JAPAN)                                    
001157         OR (DIST35-REFILL-JP)                                            
001158         OR (DIST35-FROM-AU-TO-JP)                                        
001159         MOVE 'W4063200'             TO FIL-IDPGM                         
001160         MOVE DAGENS-DATUM           TO FIL-TIREGDAT                      
001161         ADD +1                      TO FIL-IDSEKVNR                      
001162         MOVE 'W476'                 TO FIL-CT-IDSYSTEM                   
001163         MOVE 'BRJ'                  TO FIL-CT-IDPTYP                     
001164         MOVE SPACE                  TO FIL-CT-IDVTYP                     
001165         MOVE SPACE                  TO FIL-WDR701-DATA                   
001166         MOVE WS-BC-PARAMETRAR       TO FIL-WDR701-DATA                   
001167                                                                          
001168         PERFORM IMS-ISRT-WDR701                                          
001169         PERFORM S12-WDR7-FINNS                                           
001170       END-IF                                                             
001171                                                                          
001172       IF ((CDC-SE OR DDC-SE)                                             
001173          AND DIS1-IDLANDX2 = ISO-AUSTRALIEN)                             
001174         OR DIST35-CDC-AU-REFILL                                          
001175         OR DIST35-FROM-JP-TO-AU                                          
001176         MOVE 'W4063200'             TO FIL-IDPGM                         
001177         MOVE DAGENS-DATUM           TO FIL-TIREGDAT                      
001178         ADD +1                      TO FIL-IDSEKVNR                      
001179         MOVE 'W476'                 TO FIL-CT-IDSYSTEM                   
001180         MOVE 'BRA'                  TO FIL-CT-IDPTYP                     
001181         MOVE SPACE                  TO FIL-CT-IDVTYP                     
001182         MOVE SPACE                  TO FIL-WDR701-DATA                   
001183         MOVE WS-BC-PARAMETRAR       TO FIL-WDR701-DATA                   
001184                                                                          
001185         PERFORM IMS-ISRT-WDR701                                          
001186         PERFORM S12-WDR7-FINNS                                           
001187       END-IF                                                             
001188     END-IF                                                               
001189     .                                                                    
001190     EJECT                                                                
001191 DK-SKAPA-RSITRANS  SECTION.                                              
001192*    TEST FÖR VARJE KOLLI                                                 
001193*                                                                         
001194     IF DCS-FLRSI = 'J'                                                   
001195       MOVE SKOLLI-IDDISTR          TO W-IDDISTR-WDB2                     
001196       MOVE SKOLLI-IDKUNDNR         TO W-IDKUNDNR-WDB2                    
001197       IF SKOLLI-IDDISTR  NOT = GMT-IDDISTR  OR                           
001198          SKOLLI-IDKUNDNR NOT = GMT-IDKUNDNR                              
001199          PERFORM IMS-GU-WDB201                                           
001200       END-IF                                                             
001201       IF GMT-FLLDCKND = 'J'                                              
001202          MOVE 'J'                  TO FL-RSI-TRANS                       
001203       END-IF                                                             
001204     END-IF                                                               
001205     .                                                                    
001206     EJECT                                                                
001207 S01-OPEN-WZ01 SECTION.                                                   
001208     MOVE 'S01-OPEN-WZ01'       TO WS-SEKTION                             
001209                                                                          
001210     MOVE 'OPEN'                     TO SEND-KDFUNC                       
001211     MOVE 'CARPARTS.PULS.RSITRANS'   TO SEND-ADDISPABS                    
001212     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
001213                                                                          
001214     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
001215                                SEND-OPEN-AREA                            
001216     IF SEND-KDRC > 0                                                     
001217       MOVE SEND-KDRC           TO KDRC-DISP                              
001218       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
001219            DELIMITED BY SIZE INTO FELTEXT                                
001220       CALL FELLOG                                                        
001221     ELSE                                                                 
001222       MOVE SEND-IDCOM               TO WS-IDCOM                          
001223     END-IF                                                               
001224     .                                                                    
001225     EJECT                                                                
001226 S02-SEND-WZ01 SECTION.                                                   
001227     MOVE 'S02-SEND-WZ01'    TO WS-SEKTION                                
001228                                                                          
001229     MOVE 'PUT'                      TO SEND-KDFUNC                       
001230     COMPUTE SEND-KVDLEN = LENGTH OF 4630-MID-W40630I1                    
001231     MOVE MID-IDSHIPM        TO 4630-MID-IDSHIPM                          
001232                                                                          
001233     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
001234                                SEND-KVDLEN                               
001235                                4630-MID-W40630I1                         
001236     IF SEND-KDRC > 0                                                     
001237       MOVE SEND-KDRC           TO KDRC-DISP                              
001238       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
001239            DELIMITED BY SIZE INTO FELTEXT                                
001240       CALL FELLOG                                                        
001241     END-IF                                                               
001242     .                                                                    
001243     EJECT                                                                
001244 S03-CLOSE-WZ01  SECTION.                                                 
001245     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
001246                                                                          
001247     MOVE 'CLOSE'               TO SEND-KDFUNC                            
001248                                                                          
001249     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
001250     IF SEND-KDRC > 0                                                     
001251       MOVE SEND-KDRC           TO KDRC-DISP                              
001252       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
001253            DELIMITED BY SIZE INTO FELTEXT                                
001254       CALL FELLOG                                                        
001255     END-IF                                                               
001256     .                                                                    
001257     EJECT                                                                
001258 S11-RECV-OPEN  SECTION.                                                  
001259     MOVE 'S11-RECV-OPEN'          TO WS-SEKTION                          
001260                                                                          
001261     MOVE 'OPEN'                   TO RECV-KDFUNC                         
001262     MOVE 'CARPARTS.PULS.SHIPIT '  TO RECV-ADDISPABS                      
001263                                                                          
001264     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
001265                                   RECV-OPEN-AREA                         
001266                                                                          
001267     IF RECV-KDRC > 0                                                     
001268      MOVE RECV-KDRC               TO KDRC-DISP                           
001269      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
001270        DELIMITED BY SIZE INTO FELTEXT                                    
001271      CALL FELLOG                                                         
001272     END-IF                                                               
001273     .                                                                    
001274     EJECT                                                                
001275 S12-RECV-MESSAGE SECTION.                                                
001276     MOVE 'S12-RECV-MESSAGE'      TO WS-SEKTION                           
001277                                                                          
001278     MOVE 'GET'                    TO RECV-KDFUNC                         
001279     MOVE LENGTH OF MID-W40632I1   TO RECV-KVDLEN                         
001280     CALL WZ01RECV USING RECV-CONTROL-AREA                                
001281                         RECV-KVDLEN                                      
001282                         MID-W40632I1                                     
001283*                                                                         
001284     IF RECV-KDRC > 1                                                     
001285       MOVE RECV-KDRC            TO KDRC-DISP                             
001286       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
001287         DELIMITED BY SIZE INTO FELTEXT                                   
001288       CALL FELLOG                                                        
001289     END-IF                                                               
001290     .                                                                    
001291     EJECT                                                                
001292 S13-RECV-CLOSE SECTION.                                                  
001293     MOVE 'S13-RECV-CLOSE'      TO WS-SEKTION                             
001294                                                                          
001295     MOVE 'CLOSE'                TO RECV-KDFUNC                           
001296     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
001297*                                                                         
001298     IF RECV-KDRC > 0                                                     
001299       MOVE RECV-KDRC            TO KDRC-DISP                             
001300       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
001301         DELIMITED BY SIZE INTO FELTEXT                                   
001302       CALL FELLOG                                                        
001303     END-IF                                                               
001304     .                                                                    
001305     EJECT                                                                
001306 Z-FINIT SECTION.                                                         
001307     MOVE 'Z-FINIT'              TO WS-SEKTION                            
001308                                                                          
001309     IF DCS-FLRSI = 'J'                                                   
001310       IF FL-RSI-TRANS = 'J'                                              
001311         PERFORM S01-OPEN-WZ01                                            
001312         PERFORM S02-SEND-WZ01                                            
001313         PERFORM S03-CLOSE-WZ01                                           
001314       END-IF                                                             
001315     END-IF                                                               
001316                                                                          
001317     IF FL-W476ST = 'J'                                                   
001318       PERFORM ZC-SOP-W476ST                                              
001319     END-IF                                                               
001320                                                                          
001321     IF FL-W476SP = 'J'                                                   
001322       PERFORM ZD-SOP-W476SP                                              
001323     END-IF                                                               
001324     .                                                                    
001325     EJECT                                                                
001326 ZC-SOP-W476ST  SECTION.                                                  
001327     MOVE 'ZC-SOP-W476ST'     TO WS-SEKTION                               
001328                                                                          
001329     MOVE WS-IDSHIPM    TO BC-URV-IDSHIPM                                 
001330     MOVE '4632'        TO MSGSOP-IDTRANS                                 
001331     MOVE '1'           TO MSGSOP-KDMFSFOR                                
001332     MOVE 'W476ST'      TO MSGSOP-IDPROCESS                               
001333     MOVE 'O'           TO MSGSOP-KDSOPFUNK                               
001334                                                                          
001335     STRING 'URVAL(' WS-BC ')'                                            
001336             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
001337                                                                          
001338     PERFORM IMS-INSERT-ALTMSG-SOP                                        
001339     .                                                                    
001340     EJECT                                                                
001341 ZD-SOP-W476SP  SECTION.                                                  
001342     MOVE 'ZD-SOP-W476SP'     TO WS-SEKTION                               
001343                                                                          
001344     MOVE WS-IDSHIPM    TO BC-URV-IDSHIPM                                 
001345     MOVE '4632'        TO MSGSOP-IDTRANS                                 
001346     MOVE '1'           TO MSGSOP-KDMFSFOR                                
001347     MOVE 'W476SP'      TO MSGSOP-IDPROCESS                               
001348     MOVE 'O'           TO MSGSOP-KDSOPFUNK                               
001349                                                                          
001350     STRING 'IDSHIPM(' WS-BC ')'                                          
001351             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
001352                                                                          
001353     PERFORM IMS-INSERT-ALTMSG-SOP                                        
001354     .                                                                    
001355     EJECT                                                                
001356 S12-WDR7-FINNS   SECTION.                                                
001357     MOVE 'S12-WDR7-FINNS'    TO WS-SEKTION                               
001358                                                                          
001359     IF FIL-IDSEKVNR = 999                                                
001360        MOVE ZERO            TO FIL-IDSEKVNR                              
001361        ADD +1               TO FIL-TIKLOCK                               
001362     END-IF                                                               
001363                                                                          
001364     PERFORM UNTIL SEGMENT-FINNS                                          
001365        ADD +1 TO FIL-IDSEKVNR                                            
001366        PERFORM IMS-ISRT-WDR701                                           
001367        IF FIL-IDSEKVNR = 999                                             
001368          MOVE ZERO          TO FIL-IDSEKVNR                              
001369          ADD +1             TO FIL-TIKLOCK                               
001370        END-IF                                                            
001371     END-PERFORM                                                          
001372     .                                                                    
001373     EJECT                                                                
001374* --- IMS SEKTIONER ---                                                   
001375     SKIP3                                                                
001376 IMS-INSERT-ALTMSG-SOP SECTION.                                           
001377     MOVE 'IMS-INSERT-ALTMSG'  TO WS-SEKTION                              
001378     MOVE SPACE TO GODK-STATUSKODER                                       
001379     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
001380     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
001381     PERFORM IMS-STATUSKONTROLL                                           
001382     .                                                                    
001383     EJECT                                                                
001384 IMS-GU-WDE101 SECTION.                                                   
001385     MOVE 'IMS-GU-WDE101'      TO WS-SEKTION                              
001386                                                                          
001387     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
001388          DELIMITED BY SIZE INTO SSA1                                     
001389     MOVE '    ' TO GODK-STATUSKODER                                      
001390     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
001391     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
001392     PERFORM IMS-STATUSKONTROLL                                           
001393     .                                                                    
001394     EJECT                                                                
001395 IMS-GNP-WDE111 SECTION.                                                  
001396     MOVE 'IMS-GNP-WDE111'     TO WS-SEKTION                              
001397                                                                          
001398     MOVE 'WDE111  '         TO SSA1                                      
001399     MOVE '  GE' TO GODK-STATUSKODER                                      
001400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
001401     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
001402     PERFORM IMS-STATUSKONTROLL                                           
001403     .                                                                    
001404     EJECT                                                                
001405 IMS-GNP-WDE121 SECTION.                                                  
001406     MOVE 'IMS-GNP-WDE121'     TO WS-SEKTION                              
001407                                                                          
001408     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
001409            DELIMITED BY SIZE INTO SSA1                                   
001410     MOVE 'WDE121  '         TO SSA2                                      
001411     MOVE '  GE' TO GODK-STATUSKODER                                      
001412     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
001413     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
001414     PERFORM IMS-STATUSKONTROLL                                           
001415     .                                                                    
001416     EJECT                                                                
001417 IMS-ISRT-WDR701   SECTION.                                               
001418     MOVE 'IMS-ISRT-WDR701'    TO WS-SEKTION                              
001419                                                                          
001420     MOVE 'WDR701 ' TO SSA1                                               
001421     MOVE '  II' TO GODK-STATUSKODER                                      
001422     CALL CBLTDLI USING ISRT WDR7-PCB DLI-IO-WDR701 SSA1                  
001423     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
001424     PERFORM IMS-STATUSKONTROLL                                           
001425     .                                                                    
001426     EJECT                                                                
001427 IMS-GU-WDE601   SECTION.                                                 
001428     MOVE 'IMS-GU-WDE601'   TO WS-SEKTION                                 
001429                                                                          
001430     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
001431            DELIMITED BY SIZE INTO SSA1                                   
001432     MOVE '  GE' TO GODK-STATUSKODER                                      
001433     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
001434     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
001435     PERFORM IMS-STATUSKONTROLL                                           
001436     .                                                                    
001437     EJECT                                                                
001438 IMS-GU-WDB201 SECTION.                                                   
001439     MOVE 'IMS-GU-WDB201'       TO WS-SEKTION                             
001440                                                                          
001441     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
001442          DELIMITED BY SIZE INTO SSA1                                     
001443     MOVE '    '               TO GODK-STATUSKODER                        
001444     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
001445     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
001446     PERFORM IMS-STATUSKONTROLL                                           
001447     .                                                                    
001448     SKIP2                                                                
001449 IMS-GU-WDB301  SECTION.                                                  
001450     MOVE 'IMS-GU-WDB301'   TO WS-SEKTION                                 
001451                                                                          
001452     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X ')'                        
001453          DELIMITED BY SIZE INTO SSA1                                     
001454     MOVE '  GE' TO GODK-STATUSKODER                                      
001455     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
001456     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
001457     PERFORM IMS-STATUSKONTROLL                                           
001458     .                                                                    
001459     SKIP3                                                                
001460 IMS-GU-WDB601 SECTION.                                                   
001461                                                                          
001462     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001463       DELIMITED BY SIZE INTO SSA1                                        
001464     MOVE '  GE' TO GODK-STATUSKODER                                      
001465     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
001466     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001467     PERFORM IMS-STATUSKONTROLL                                           
001468     IF SEGMENT-SAKNAS                                                    
001469       MOVE SPACE TO DCS-KDDC                                             
001470     END-IF                                                               
001471     .                                                                    
001472     SKIP2                                                                
001473 IMS-ISRT-WDGX4503-04       SECTION.                                      
001474                                                                          
001475     STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
001476         DELIMITED BY SIZE INTO SSA1                                      
001477     MOVE 'WDGX4504'       TO   SSA2                                      
001478     MOVE '  GE' TO GODK-STATUSKODER                                      
001479     CALL CBLTDLI USING ISRT 4503-PCB 4504-WDGX4504 SSA1 SSA2             
001480     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
001481     PERFORM IMS-STATUSKONTROLL                                           
001482     .                                                                    
001483     SKIP3                                                                
001484                                                                          
001485 IMS-STATUSKONTROLL SECTION.                                              
001486                                                                          
001487     SET STATUS-IX TO 1                                                   
001488     SEARCH GODK-STATUS                                                   
001489       AT END                                                             
001490         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001491         DELIMITED BY SIZE INTO FELTEXT                                   
001492         CALL FELLOG                                                      
001493       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001494         CONTINUE                                                         
001495     END-SEARCH                                                           
001500     .                                                                    
