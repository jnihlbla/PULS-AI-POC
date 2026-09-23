000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W4034400.                                                
000004 AUTHOR.         LENA BROMANDER.                                          
000005 DATE-WRITTEN.   OKT 2016.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        MPP SOM VISAR ANTINGEN                                           
000010*        1. VILKA KOLLIN SOM INGÅR I ETT SAMLINGSKOLLI                    
000011*        ELLER                                                            
000012*        2. VILKET SAMLINGSKOLLI ETT KOLLI TILLHÖR                        
000013*        ELLER                                                            
000014*        3. ÅTERÖPPNADE SAMLINGSKOLLIN                                    
000015*                                                                         
000016*        SÖKNING SKER MED NYCKELN:                                        
000017*        1. SAMLINGSKOLLI + DC                                            
000018*        ELLER                                                            
000019*        2. DIST + KUNDNR + ORDER + KOLLI + DC                            
000020*        ELLER                                                            
000021*        3. INGEN NYCKEL VID SÖKNING SK I R-STATUS                        
000022*           SÄTT FLAGGA = J                                               
000023*                                                                         
000024*        PROGRAMMET LÄSER      WDE7                                       
000025*                                                                         
000026*    INDATA.                                                              
000027*        TRANSAKTION: W4T344                                              
000028*        MID:         W4I34401                                            
000029*                                                                         
000030*    UTDATA.                                                              
000031*        MOD:         W4O34401                                            
000032                                                                          
000033                                                                          
000034 ENVIRONMENT DIVISION.                                                    
000035     EJECT                                                                
000036 DATA DIVISION.                                                           
000037 WORKING-STORAGE SECTION.                                                 
000038                                                                          
000039*    -- CHECKED BY WY2000                                                 
000040 77  IDPGM                       PIC X(08)   VALUE 'W4034400'.            
000041 77  CURRENT-SECTION             PIC X(24)   VALUE SPACE.                 
000042 77  CURRENT-IMS-SECTION         PIC X(24)   VALUE SPACE.                 
000043                                                                          
000044*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000045 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000046                                                                          
000047 77  JA                          PIC X       VALUE 'J'.                   
000048 77  YES                         PIC X       VALUE 'Y'.                   
000049 77  NEJ                         PIC X       VALUE 'N'.                   
000050                                                                          
000051*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000052 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000053 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000054 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
000055 77  INDX-R                      PIC S9(4)  VALUE +0    COMP SYNC.        
000056 77  MAX-INDX-R                  PIC S9(4)  VALUE +7    COMP SYNC.        
000057 77  MAX-3-INDX-R                PIC S9(4)  VALUE +3    COMP SYNC.        
000058*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000059*                                                                         
000060*      --- VALID IDDC CODES                                               
000061*                                                                         
000062*01    -COPY WWDC99                                                       
000063                                                                          
000064       EJECT                                                              
000065 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000066     88  NYCKLAR-OK                          VALUE 'J'.                   
000067     88  NYCKLAR-FEL                         VALUE 'N'.                   
000068                                                                          
000069 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000070     88  EGEN-MID                            VALUE '4344'.                
000071     88  GODK-MID                            VALUE '4341' '4342'          
000072                                                   '4343' '4344'          
000073                                                   '4345' '4346'          
000074                                                   '4347' '4348'          
000075                                                   '4349'.                
000076     88  HELP-MID                            VALUE '0551'.                
000077                                                                          
000078 77  SOEK-INGAENDE-KOLLI-SW      PIC X       VALUE ' '.                   
000079     88  SOEK-INGAENDE-KOLLI                 VALUE 'J'.                   
000080     88  SOEK-KOLLI-SAMP                     VALUE 'N'.                   
000081     88  SOEK-REOPENED-SK                    VALUE 'R'.                   
000082     88  SOEK-VILLKOR-EJ-VALT                VALUE ' '.                   
000083                                                                          
000084 01  ARBETSFAELT.                                                         
000085     03  WS-IDKOLLI-SAMP         PIC S9(5)   VALUE ZERO COMP-3.           
000086     03  WS-DATUM-TID.                                                    
000087       05 WS-DATUM               PIC X(8)    VALUE SPACE.                 
000088       05 WS-KLOCKAN             PIC 9(10)   VALUE ZERO.                  
000089     03  W-MSGI-IDKUNDRF.                                                 
000090       05 W-MSGI-IDORDNR7        PIC X(7).                                
000091       05 FILLER                 PIC X(3)    VALUE SPACE.                 
000092     03  WS-IDDC-IN              PIC X(2)    VALUE SPACE.                 
000093                                                                          
000094     EJECT                                                                
000095*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000096 01  GENERELLA-SUBPROGRAM.                                                
000097     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000098     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000099     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000101     EJECT                                                                
000102*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000103*01 -COPY WMEDAREA                                                        
000104                                                                          
000105 01  MESSAGE-CODES.                                                       
000106     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000107     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
000108     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000109     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000110     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000111     03  THIS-IS-THE-LAST-PAGE   PIC X(3)    VALUE '106'.                 
000112     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000113     EJECT                                                                
000114*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000115*                                                                         
000116 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000117                                                                          
000118*01 -COPY WMSGINIT                                                        
000119                                                                          
000120*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000121*                                                                         
000122 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000123                                                                          
000124*01  MID -COPY W4I34401                                                   
000125     EJECT                                                                
000126 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000127                                                                          
000128*01  -COPY WMSGAREA                                                       
000129     EJECT                                                                
000130     03  MOD REDEFINES MSG-AREA.                                          
000131*      05  -COPY W4O34401                                                 
000132     EJECT                                                                
000133 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000134                                                                          
000135*01  -COPY WMFSAREA                                                       
000136     EJECT                                                                
000137*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000138*                                                                         
000139     EJECT                                                                
000140 01  FILLER                      PIC X(16)    VALUE 'IMS-WS'.             
000141                                                                          
000142 01  SPAR-AREA.                                                           
000143     03  SPAR-IDTRANS            PIC X(4)     VALUE SPACE.                
000144                                                                          
000145     03  SPAR-WDE7ASEQ-ENTER.                                             
000146         05  SPAR-IDDC-ENTER      PIC X(2)     VALUE SPACE.               
000147         05  SPAR-IDKOLLI-SAMP-ENTER                                      
000148                                  PIC S9(5)    VALUE ZERO COMP-3.         
000149     03  SPAR-WDE7B1KY-ENTER.                                             
000150         05 SPAR-IDDISTR-ENTER    PIC S9(5)    VALUE ZERO COMP-3.         
000151         05 SPAR-IDKUNDNR-ENTER   PIC S9(7)    VALUE ZERO COMP-3.         
000152         05 SPAR-IDORDNR7-ENTER   PIC 9(7)     VALUE ZERO.                
000153         05 SPAR-IDKOLLI-ENTER    PIC S9(5)    VALUE ZERO COMP-3.         
000154                                                                          
000155     03  SPAR-WDE7C1KY-ENTER.                                             
000156         05 SPAR-TIREGDAT-R-ENTER PIC S9(7)   VALUE ZERO  COMP-3.         
000157         05 SPAR-IDDC-R-ENTER     PIC X(2)    VALUE SPACE.                
000158         05 SPAR-IDKOLLI-SAMP-R-ENTER                                     
000159                                  PIC S9(5)   VALUE ZERO COMP-3.          
000160                                                                          
000161     03  SPAR-WDE7ASEQ-NEXT.                                              
000162         05 SPAR-IDDC-NEXT        PIC X(2)     VALUE SPACE.               
000163         05 SPAR-IDKOLLI-SAMP-NEXT                                        
000164                                  PIC S9(5)     VALUE ZERO COMP-3.        
000165     03  SPAR-WDE7B1KY-NEXT.                                              
000166         05 SPAR-IDDISTR-NEXT     PIC S9(5)    VALUE ZERO  COMP-3.        
000167         05 SPAR-IDKUNDNR-NEXT    PIC S9(7)    VALUE ZERO  COMP-3.        
000168         05 SPAR-IDORDNR7-NEXT    PIC 9(7)     VALUE ZERO.                
000169         05 SPAR-IDKOLLI-NEXT     PIC S9(5)    VALUE ZERO COMP-3.         
000170                                                                          
000171     03  SPAR-WDE7C1KY-NEXT.                                              
000172         05 SPAR-TIREGDAT-R-NEXT  PIC S9(7)   VALUE ZERO  COMP-3.         
000173         05 SPAR-IDDC-R-NEXT      PIC X(2)    VALUE SPACE.                
000174         05 SPAR-IDKOLLI-SAMP-R-NEXT                                      
000175                                  PIC S9(5)   VALUE ZERO COMP-3.          
000176                                                                          
000177     03  SPAR-SOEK-INGAENDE-KOLLI-SW  PIC X    VALUE 'N'.                 
000178                                                                          
000179                                                                          
000180                                                                          
000181 01  NYCKLAR-TILL-DLI.                                                    
000182     03  W-WDE7ASEQ-X.                                                    
000183         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
000184         05 W-IDKOLLI-SAMP       PIC S9(5)   VALUE ZERO  COMP-3.          
000185                                                                          
000186     03  W-WDE721KY-MIN-X.                                                
000187         05 W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
000188         05 W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
000189         05 W-IDORDNR7-MIN       PIC 9(7)    VALUE ZERO.                  
000190         05 W-IDKOLLI-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
000191                                                                          
000192     03  W-WDE721KY-MAX-X.                                                
000193         05 W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
000194         05 W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
000195         05 W-IDORDNR7-MAX       PIC 9(7)    VALUE ZERO.                  
000196         05 W-IDKOLLI-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
000197                                                                          
000198     03  W-WDE7B1KY-X.                                                    
000199         05 W-7B1-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
000200         05 W-7B1-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000201         05 W-7B1-IDORDNR7       PIC 9(7)    VALUE ZERO.                  
000202         05 W-7B1-IDKOLLI        PIC S9(5)   VALUE ZERO  COMP-3.          
000203                                                                          
000204     03  W-WDE7C1KY-X.                                                    
000205         05 W-7C1-TIREGDAT       PIC S9(7)   VALUE ZERO  COMP-3.          
000206         05 W-7C1-IDDC           PIC X(2)    VALUE SPACE.                 
000207         05 W-7C1-IDKOLLI-SAMP   PIC S9(5)   VALUE ZERO  COMP-3.          
000208                                                                          
000209                                                                          
000210*    --- STATUS-KOD FRÅN IMS                                              
000211 01  STATUS-WS                   PIC XX.                                  
000212     88  SEGMENT-FINNS                       VALUE '  '.                  
000213     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000214     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000215     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000216                                                                          
000217 01  GODK-STATUSKODER.                                                    
000218     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000219                                                                          
000220 01  SSA1                        PIC X(96).                               
000221 01  SSA2                        PIC X(96).                               
000222     EJECT                                                                
000223*    --- IMS FUNKTIONSKODER                                               
000224*01  -COPY W0003                                                          
000225     EJECT                                                                
000226*    ---  DLI INPUT-OUTPUT AREA                                           
000227                                                                          
000228 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE711'.                      
000229 01  DLI-IO-WDE711.                                                       
000230*    03  -COPY WDE711                                                     
000231                                                                          
000232 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE721'.                      
000233 01  DLI-IO-WDE721.                                                       
000234*    03  -COPY WDE721                                                     
000235                                                                          
000236 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE7A1'.                      
000237 01  DLI-IO-WDE7A1.                                                       
000238*    03  -COPY WDE7A1                                                     
000239                                                                          
000240 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE7B1'.                      
000241 01  DLI-IO-WDE7B1.                                                       
000242*    03  -COPY WDE7B1                                                     
000243                                                                          
000244 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE7C1'.                      
000245 01  DLI-IO-WDE7C1.                                                       
000246*    03  -COPY WDE7C1                                                     
000247                                                                          
000248     EJECT                                                                
000249 LINKAGE SECTION.                                                         
000250                                                                          
000251*01  -COPY W0009   -PRE MSG-                                              
000252*01  -COPY W0008   -PRE WDP7-                                             
000253     05  FILLER                  PIC X.                                   
000254     EJECT                                                                
000255*01  -COPY W0008  -PRE WDE7-                                              
000256     05  FILLER                  PIC X.                                   
000257     EJECT                                                                
000258*01  -COPY W0008  -PRE WDE7B-                                             
000259     05  FILLER                  PIC X.                                   
000260     EJECT                                                                
000261*01  -COPY W0008  -PRE WDE7C-                                             
000262     05  FILLER                  PIC X.                                   
000263     EJECT                                                                
000264                                                                          
000265 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
000266                                   WDE7-PCB                               
000267                                   WDE7B-PCB                              
000268                                   WDE7C-PCB.                             
000269 MAIN SECTION.                                                            
000270     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
000271                                   WDE7-PCB                               
000272                                   WDE7B-PCB                              
000273                                   WDE7C-PCB.                             
000274                                                                          
000275     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
000276     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
000277                                                                          
000278     PERFORM IMS-GET-MSG                                                  
000279     IF SEGMENT-FINNS                                                     
000280       PERFORM A-INIT                                                     
000281       IF EGEN-MID                                                        
000282         PERFORM B-KOLLA-NYCKLAR                                          
000283         IF NYCKLAR-OK                                                    
000284             IF MFS-FIRST                                                 
000285               PERFORM C-FOERSTA-SIDA                                     
000286             ELSE                                                         
000287               IF MFS-NEXT                                                
000288                 PERFORM D-NAESTA-SIDA                                    
000289               ELSE                                                       
000290                 PERFORM E-SAMMA-SIDA                                     
000291               END-IF                                                     
000292             END-IF                                                       
000293           PERFORM F-LAES-VISA-INFO                                       
000294         END-IF                                                           
000295       END-IF                                                             
000296       PERFORM IMS-INSERT-MSG                                             
000297     END-IF                                                               
000298                                                                          
000299     MOVE ZERO TO RETURN-CODE                                             
000300     GOBACK                                                               
000301     .                                                                    
000302     EJECT                                                                
000303 A-INIT SECTION.                                                          
000304     MOVE 'A-INIT             '    TO CURRENT-SECTION                     
000305                                                                          
000306     IF MSG-DUBBLA-TRANSKODER                                             
000307       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I34401                 
000308       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000309       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000310     ELSE                                                                 
000311       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I34401                  
000312       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000313       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000314     END-IF                                                               
000315                                                                          
000316     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000317     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000318     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000319                                                                          
000320     MOVE LOW-VALUE TO MSG-AREA                                           
000321     MOVE 'W4O344N1' TO MFS-IDMOD                                         
000322     MOVE '4344' TO MOD-IDTRANS                                           
000323     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000324                                                                          
000325*    --- OM SVAR TILL SKÄRM:        MSG-KVLL = MOD-LÄNGD + 4              
000326*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:     = MOD-LÄNGD + 17             
000327     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O34401 + 4                        
000328                                                                          
000329     IF EGEN-MID OR HELP-MID                                              
000330       CONTINUE                                                           
000331     ELSE                                                                 
000332       MOVE SPACE TO MFS-KDTRTYP                                          
000333       MOVE '7' TO MFS-IDPFK                                              
000334*      VI TAR INTE IN EV NYCKLAR FRÅN ANNAN TRANS                         
000335       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP-IN                        
000336                             MOD-IDDC-IN                                  
000337                             MOD-IDDISTR-IN                               
000338                             MOD-IDKUNDNR-IN                              
000339                             MOD-IDORDNR7-IN                              
000340                             MOD-IDKOLLI-IN                               
000341     END-IF                                                               
000342                                                                          
000343     MOVE ZERO           TO WS-IDKOLLI-SAMP                               
000344     MOVE LOW-VALUE      TO W-WDE721KY-MIN-X                              
000345     MOVE HIGH-VALUE     TO W-WDE721KY-MAX-X                              
000346     MOVE ZERO           TO W-7C1-TIREGDAT                                
000347                            W-7C1-IDKOLLI-SAMP                            
000348     MOVE SPACE          TO W-7C1-IDDC                                    
000349                                                                          
000350     .                                                                    
000351     EJECT                                                                
000352 B-KOLLA-NYCKLAR SECTION.                                                 
000353     MOVE 'B-KOLLA-NYCKLAR    '    TO CURRENT-SECTION                     
000354                                                                          
000355*     SKA VI HÄMTA FRÅN MSGI I DETTA PGM?                                 
000356     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000357     MOVE '001'             TO MSGI-KDCALL                                
000358     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000359     MOVE '4344'            TO MSGI-IDTRANS                               
000360     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000361                                                                          
000362     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000363                                                                          
000364     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
000365                                                                          
000366     IF MSGI-IDLAND-SPR = 'GB'                                            
000367       MOVE +2 TO SPRAK-IX                                                
000368     ELSE                                                                 
000369       MOVE +1 TO SPRAK-IX                                                
000370     END-IF                                                               
000371                                                                          
000372     MOVE JA TO NYCKLAR-SW                                                
000373                                                                          
000374     MOVE MSGI-IDLAND-SPR       TO MED-IDSKYLT                            
000375*LB  MOVE MSGI-IDDC             TO W-IDDC                                 
000376                                                                          
000377*    -- KONTROLL AV INMATAT                                               
000378     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP-IN                          
000379                             MOD-IDDC-IN                                  
000380                             MOD-IDDISTR-IN                               
000381                             MOD-IDKUNDNR-IN                              
000382                             MOD-IDORDNR7-IN                              
000383                             MOD-IDKOLLI-IN                               
000384*                                                                         
000385     IF MID-IDKOLLI-SAMP-IN NOT = ALL '+' OR                              
000386       MID-IDDC-IN          NOT = ALL '+' OR                              
000387       MID-IDKUNDNR-IN      NOT = ALL '+' OR                              
000388       MID-IDDISTR-IN       NOT = ALL '+' OR                              
000389       MID-IDORDNR7-IN      NOT = ALL '+' OR                              
000390       MID-IDKOLLI-IN       NOT = ALL '+'                                 
000391*                                                                         
000392       MOVE '7'         TO MFS-IDPFK                                      
000393       MOVE SPACE       TO MFS-KDTRTYP                                    
000394     END-IF                                                               
000395                                                                          
000396*    --  IDDISTR                                                          
000397                                                                          
000398     IF MID-IDDISTR-IN  = ALL '+'                                         
000399       IF  MID-IDDISTR-UT NOT = ALL '+'                                   
000400         MOVE MID-IDDISTR-UT     TO MID-IDDISTR-IN                        
000401       END-IF                                                             
000402     ELSE                                                                 
000403       MOVE NEJ                 TO SOEK-INGAENDE-KOLLI-SW                 
000404     END-IF                                                               
000405                                                                          
000406     IF MID-IDDISTR-IN  NUMERIC                                           
000407       IF MID-IDDISTR-IN > ZERO                                           
000408         MOVE MID-IDDISTR-IN       TO W-7B1-IDDISTR                       
000409       END-IF                                                             
000410     END-IF                                                               
000411                                                                          
000412*    --  IDKUNDNR                                                         
000413                                                                          
000414     IF MID-IDKUNDNR-IN  = ALL '+'                                        
000415       IF  MID-IDKUNDNR-UT NOT = ALL '+'                                  
000416         MOVE MID-IDKUNDNR-UT     TO MID-IDKUNDNR-IN                      
000417       END-IF                                                             
000418     ELSE                                                                 
000419       MOVE NEJ                 TO SOEK-INGAENDE-KOLLI-SW                 
000420     END-IF                                                               
000421                                                                          
000422     IF MID-IDKUNDNR-IN NUMERIC                                           
000423       IF MID-IDKUNDNR-IN = ZERO OR                                       
000424          MID-IDKUNDNR-IN > ZERO                                          
000425         MOVE MID-IDKUNDNR-IN     TO W-7B1-IDKUNDNR                       
000426       END-IF                                                             
000427     END-IF                                                               
000428                                                                          
000429*    --  IDORDNR                                                          
000430                                                                          
000431     IF MID-IDORDNR7-IN  = ALL '+'                                        
000432       IF  MID-IDORDNR7-UT NOT = ALL '+'                                  
000433         MOVE MID-IDORDNR7-UT     TO MID-IDORDNR7-IN                      
000434       END-IF                                                             
000435     ELSE                                                                 
000436       MOVE NEJ                 TO SOEK-INGAENDE-KOLLI-SW                 
000437     END-IF                                                               
000438                                                                          
000439     IF MID-IDORDNR7-IN NUMERIC AND                                       
000440       MID-IDORDNR7-IN > ZERO                                             
000441       MOVE MID-IDORDNR7-IN        TO W-7B1-IDORDNR7                      
000442     END-IF                                                               
000443                                                                          
000444*    --  IDKOLLI                                                          
000445                                                                          
000446     IF MID-IDKOLLI-IN  = ALL '+'                                         
000447       IF  MID-IDKOLLI-UT NOT = ALL '+'                                   
000448         MOVE MID-IDKOLLI-UT     TO MID-IDKOLLI-IN                        
000449       END-IF                                                             
000450     ELSE                                                                 
000451       MOVE NEJ                 TO SOEK-INGAENDE-KOLLI-SW                 
000452     END-IF                                                               
000453                                                                          
000454     IF MID-IDKOLLI-IN  NUMERIC AND                                       
000455       MID-IDKOLLI-IN  > ZERO                                             
000456       MOVE MID-IDKOLLI-IN         TO W-7B1-IDKOLLI                       
000457     END-IF                                                               
000458                                                                          
000459*    --  IDKOLLI-SAMP                                                     
000460                                                                          
000461     IF MID-IDKOLLI-SAMP-IN  = ALL '+'                                    
000462       IF  MID-IDKOLLI-SAMP-UT NOT = ALL '+'                              
000463         MOVE MID-IDKOLLI-SAMP-UT     TO MID-IDKOLLI-SAMP-IN              
000464       END-IF                                                             
000465     ELSE                                                                 
000466       MOVE JA                  TO SOEK-INGAENDE-KOLLI-SW                 
000467     END-IF                                                               
000491                                                                          
000492     IF MID-IDKOLLI-SAMP-IN NOT = ALL '+' AND                             
000493        MID-IDKOLLI-SAMP-IN     > SPACES                                  
000494        IF MID-IDKOLLI-SAMP-IN NUMERIC                                    
000495           IF MID-IDKOLLI-SAMP-IN > ZERO                                  
000496              MOVE MID-IDKOLLI-SAMP-IN   TO W-IDKOLLI-SAMP                
000497           ELSE                                                           
000498              MOVE NEJ                   TO NYCKLAR-SW                    
000499           END-IF                                                         
000500        ELSE                                                              
000501           MOVE NEJ                      TO NYCKLAR-SW                    
000502        END-IF                                                            
000503     END-IF                                                               
000504                                                                          
000507*--- IDDC                                                                 
000508                                                                          
000509     MOVE MSGI-IDDC           TO WS-IDDC                                  
000510                                                                          
000511     IF CDC-SE                                                            
000512        IF MID-IDDC-IN NOT = ALL '+'                                      
000513          MOVE MID-IDDC-IN      TO WS-IDDC                                
000514          IF CDC-SE OR DDC-SE                                             
000515             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
000516             MOVE '7'           TO MFS-IDPFK                              
000517             MOVE SPACE         TO MFS-KDTRTYP                            
000518          ELSE                                                            
000519             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
000520          END-IF                                                          
000521        ELSE                                                              
000522          MOVE MID-IDDC-UT      TO WS-IDDC                                
000523          IF DDC-SE                                                       
000524             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
000525          ELSE                                                            
000526             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
000527          END-IF                                                          
000528        END-IF                                                            
000529     ELSE                                                                 
000530        MOVE MSGI-IDDC          TO WS-IDDC-IN                             
000531     END-IF                                                               
000532                                                                          
000533     MOVE WS-IDDC-IN            TO W-IDDC                                 
000534                                   MOD-IDDC-UT                            
000535                                                                          
000536*--- FLREOPENED        (SKA LIGGA SIST,SLÅR UT ÖVR SÖKNING)               
000537                                                                          
000538     IF MID-FLREOPENED = '+' OR NEJ OR SPACE                              
000539       MOVE NEJ                    TO MOD-FLREOPENED                      
000540       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLREOPENED-ATTR                 
000541     ELSE                                                                 
000542                                                                          
000543       IF MID-FLREOPENED  = JA OR YES                                     
000544         MOVE 'R'                   TO SOEK-INGAENDE-KOLLI-SW             
000545         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLREOPENED-ATTR                
000546       ELSE                                                               
000547         MOVE NEJ                  TO NYCKLAR-SW                          
000548         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLREOPENED-ATTR                 
000549       END-IF                                                             
000550                                                                          
000551       MOVE MID-FLREOPENED         TO MOD-FLREOPENED                      
000552     END-IF                                                               
000553                                                                          
000554*--- VI KOLLAR ENDAST NYCKLAR OM MAN EJ SÖKT PÅ R-STATUS KOLLIN           
000555                                                                          
000556     IF NYCKLAR-OK AND                                                    
000557       NOT SOEK-REOPENED-SK                                               
000558       IF MID-IDKOLLI-SAMP-IN NUMERIC AND                                 
000559         MID-IDKOLLI-SAMP-IN > ZERO                                       
000560         CONTINUE                                                         
000561       ELSE                                                               
000562         IF MID-IDDISTR-IN NOT NUMERIC OR                                 
000563           (MID-IDDISTR-IN NUMERIC AND                                    
000564           MID-IDDISTR-IN NOT > ZERO)                                     
000565           MOVE NEJ  TO NYCKLAR-SW                                        
000566         END-IF                                                           
000567                                                                          
000568         IF MID-IDKUNDNR-IN NUMERIC AND                                   
000569           (MID-IDKUNDNR-IN = ZERO  OR                                    
000570           MID-IDKUNDNR-IN > ZERO)                                        
000571           CONTINUE                                                       
000572         ELSE                                                             
000573           MOVE NEJ  TO NYCKLAR-SW                                        
000574         END-IF                                                           
000575                                                                          
000576         IF MID-IDORDNR7-IN NOT NUMERIC OR                                
000577           (MID-IDORDNR7-IN NUMERIC AND                                   
000578           MID-IDORDNR7-IN NOT > ZERO)                                    
000579           MOVE NEJ  TO NYCKLAR-SW                                        
000580         END-IF                                                           
000581                                                                          
000582*        ANTINGEN IDKOLLI-SAMP IFYLLD ELLER DISTR-KUND-ORDER-KOLLI        
000583         IF MID-IDKOLLI-IN NOT NUMERIC OR                                 
000584           (MID-IDKOLLI-IN NUMERIC AND                                    
000585           MID-IDKOLLI-IN NOT > ZERO)                                     
000586           MOVE NEJ  TO NYCKLAR-SW                                        
000587         END-IF                                                           
000588       END-IF                                                             
000589     END-IF                                                               
000590                                                                          
000591*    HAR MAN EJ ANGETT NYA NYCKLAR                                        
000592*    KOLLA I NU HOPSLAGNA MID-FALT-IN + MID-FALT-UT                       
000593*    VAD MAN VILL SÖKA PÅ                                                 
000594                                                                          
000595     IF SOEK-VILLKOR-EJ-VALT                                              
000596       IF MID-FLREOPENED = JA OR YES                                      
000597         MOVE 'R'               TO SOEK-INGAENDE-KOLLI-SW                 
000598       ELSE                                                               
000599         IF MID-IDKOLLI-SAMP-IN NUMERIC AND                               
000600           MID-IDKOLLI-SAMP-IN > ZERO                                     
000601           MOVE JA              TO SOEK-INGAENDE-KOLLI-SW                 
000602         ELSE                                                             
000603           MOVE NEJ             TO SOEK-INGAENDE-KOLLI-SW                 
000604         END-IF                                                           
000605       END-IF                                                             
000606     END-IF                                                               
000607                                                                          
000608                                                                          
000609*LÄGG UT ALLA NYCKLAR                                                     
000610     IF GODK-MID OR NYCKLAR-OK                                            
000611                                                                          
000612       IF SOEK-INGAENDE-KOLLI                                             
000613*        NYCKLAR EJ RELEVANTA VID DENNA SÖKNING RENSAS                    
000614         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
000615                                 MOD-IDKUNDNR-UT                          
000616                                 MOD-IDORDNR7-UT                          
000617                                 MOD-IDKOLLI-UT                           
000618       ELSE                                                               
000619         MOVE MID-IDDISTR-IN    TO MOD-IDDISTR-UT                         
000620                                                                          
000621         MOVE MID-IDKUNDNR-IN   TO MOD-IDKUNDNR-UT                        
000622         MOVE MID-IDORDNR7-IN   TO MOD-IDORDNR7-UT                        
000623         MOVE MID-IDKOLLI-IN    TO MOD-IDKOLLI-UT                         
000624       END-IF                                                             
000625                                                                          
000626       MOVE MID-IDKOLLI-SAMP-IN TO MOD-IDKOLLI-SAMP-UT                    
000627*LB    MOVE MSGI-IDDC           TO MOD-IDDC-UT                            
000628       MOVE WS-IDDC-IN          TO MOD-IDDC-UT                            
000629     ELSE                                                                 
000630       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP-UT                        
000631                               MOD-IDDC-UT                                
000632                               MOD-IDDISTR-UT                             
000633                               MOD-IDKUNDNR-UT                            
000634                               MOD-IDORDNR7-UT                            
000635                               MOD-IDKOLLI-UT                             
000636     END-IF                                                               
000637                                                                          
000638     IF NYCKLAR-FEL                                                       
000639       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000640       CALL WMEDKONV USING MED-WMEDAREA                                   
000641       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000642       PERFORM MFS-RENSA-FAELT-IN                                         
000643       PERFORM MFS-RENSA-FAELT-UT                                         
000644     END-IF                                                               
000645                                                                          
000646     .                                                                    
000647     EJECT                                                                
000648 C-FOERSTA-SIDA SECTION.                                                  
000649     MOVE 'C-FOERSTA-SIDA     '    TO CURRENT-SECTION                     
000650                                                                          
000651     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
000652     CALL WMEDKONV USING MED-WMEDAREA                                     
000653     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
000654                                                                          
000655*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
000656     PERFORM MFS-RENSA-FAELT-IN                                           
000657     .                                                                    
000658     EJECT                                                                
000659                                                                          
000660                                                                          
000661 D-NAESTA-SIDA SECTION.                                                   
000662     MOVE 'D-NAESTA-SIDA     '    TO CURRENT-SECTION                      
000663                                                                          
000664     IF SPAR-IDTRANS = '4344'                                             
000665        IF SPAR-IDKOLLI-SAMP-NEXT > ZERO                                  
000666          MOVE SPAR-IDKOLLI-SAMP-NEXT                                     
000667                              TO W-IDKOLLI-SAMP                           
000668        ELSE                                                              
000669          MOVE SPAR-IDKOLLI-SAMP-ENTER                                    
000670                              TO W-IDKOLLI-SAMP                           
000671        END-IF                                                            
000672                                                                          
000673        IF SPAR-IDDC-NEXT NOT = SPACE                                     
000674          MOVE SPAR-IDDC-NEXT                                             
000675                              TO W-IDDC                                   
000676        ELSE                                                              
000677          MOVE SPAR-IDDC-ENTER                                            
000678                              TO W-IDDC                                   
000679        END-IF                                                            
000680                                                                          
000681        IF SPAR-IDDISTR-NEXT > ZERO                                       
000682          MOVE SPAR-IDDISTR-NEXT                                          
000683                              TO W-IDDISTR-MIN                            
000684        ELSE                                                              
000685          MOVE SPAR-IDDISTR-ENTER                                         
000686                              TO W-IDDISTR-MIN                            
000687        END-IF                                                            
000688                                                                          
000689        IF SPAR-IDKUNDNR-NEXT > ZERO                                      
000690          MOVE SPAR-IDKUNDNR-NEXT                                         
000691                              TO W-IDKUNDNR-MIN                           
000692        ELSE                                                              
000693          MOVE SPAR-IDKUNDNR-ENTER                                        
000694                              TO W-IDKUNDNR-MIN                           
000695        END-IF                                                            
000696                                                                          
000697        IF SPAR-IDORDNR7-NEXT > ZERO                                      
000698          MOVE SPAR-IDORDNR7-NEXT                                         
000699                              TO W-IDORDNR7-MIN                           
000700        ELSE                                                              
000701          MOVE SPAR-IDORDNR7-ENTER                                        
000702                              TO W-IDORDNR7-MIN                           
000703        END-IF                                                            
000704                                                                          
000705        IF SPAR-IDKOLLI-NEXT > ZERO                                       
000706          MOVE SPAR-IDKOLLI-NEXT                                          
000707                              TO W-IDKOLLI-MIN                            
000708        ELSE                                                              
000709          MOVE SPAR-IDKOLLI-ENTER                                         
000710                              TO W-IDKOLLI-MIN                            
000711        END-IF                                                            
000712                                                                          
000713*---    NEXT-NYCKLAR FÖR SAMKOLLIN I R-STATUS                             
000714                                                                          
000715        IF SPAR-TIREGDAT-R-NEXT > ZERO                                    
000716          MOVE SPAR-TIREGDAT-R-NEXT                                       
000717                              TO W-7C1-TIREGDAT                           
000718        ELSE                                                              
000719          MOVE SPAR-TIREGDAT-R-ENTER                                      
000720                              TO W-7C1-TIREGDAT                           
000721        END-IF                                                            
000722                                                                          
000723        IF SPAR-IDDC-R-NEXT NOT = SPACE                                   
000724          MOVE SPAR-IDDC-R-NEXT                                           
000725                              TO W-7C1-IDDC                               
000726        ELSE                                                              
000727          MOVE SPAR-IDDC-R-ENTER                                          
000728                              TO W-7C1-IDDC                               
000729        END-IF                                                            
000730                                                                          
000731        IF SPAR-IDKOLLI-SAMP-R-NEXT > ZERO                                
000732          MOVE SPAR-IDKOLLI-SAMP-R-NEXT                                   
000733                              TO W-7C1-IDKOLLI-SAMP                       
000734        ELSE                                                              
000735          MOVE SPAR-IDKOLLI-SAMP-R-ENTER                                  
000736                              TO W-7C1-IDKOLLI-SAMP                       
000737        END-IF                                                            
000738     ELSE                                                                 
000739        MOVE ZERO             TO W-IDKOLLI-SAMP                           
000740                                 W-IDDISTR-MIN                            
000741                                 W-IDKUNDNR-MIN                           
000742                                 W-IDORDNR7-MIN                           
000743                                 W-IDKOLLI-MIN                            
000744                                 W-7C1-TIREGDAT                           
000745                                 W-7C1-IDKOLLI-SAMP                       
000746        MOVE SPACE            TO W-7C1-IDDC                               
000747        MOVE SPAR-SOEK-INGAENDE-KOLLI-SW                                  
000748                              TO SOEK-INGAENDE-KOLLI-SW                   
000749        PERFORM MFS-RENSA-FAELT-IN                                        
000750     END-IF                                                               
000751     .                                                                    
000752     EJECT                                                                
000753 E-SAMMA-SIDA SECTION.                                                    
000754     MOVE 'E-SAMMA-SIDA     '    TO CURRENT-SECTION                       
000755                                                                          
000756     IF EGEN-MID OR HELP-MID                                              
000757       IF SPAR-IDTRANS = '4344'                                           
000758          IF SPAR-IDKOLLI-SAMP-ENTER NUMERIC                              
000759            MOVE SPAR-IDKOLLI-SAMP-ENTER  TO W-IDKOLLI-SAMP               
000760          ELSE                                                            
000761            MOVE ZERO                     TO W-IDKOLLI-SAMP               
000762          END-IF                                                          
000763          IF SPAR-IDDC-ENTER NUMERIC                                      
000764            MOVE SPAR-IDDC-ENTER  TO W-IDDC                               
000765          ELSE                                                            
000766            MOVE ZERO             TO W-IDDC                               
000767          END-IF                                                          
000768          IF SPAR-IDDISTR-ENTER NUMERIC                                   
000769            MOVE SPAR-IDDISTR-ENTER TO W-IDDISTR-MIN                      
000770          ELSE                                                            
000771            MOVE ZERO           TO W-IDDISTR-MIN                          
000772          END-IF                                                          
000773          IF SPAR-IDKUNDNR-ENTER NUMERIC                                  
000774            MOVE SPAR-IDKUNDNR-ENTER  TO W-IDKUNDNR-MIN                   
000775          ELSE                                                            
000776            MOVE ZERO                 TO W-IDKUNDNR-MIN                   
000777          END-IF                                                          
000778          IF SPAR-IDORDNR7-ENTER NUMERIC                                  
000779            MOVE SPAR-IDORDNR7-ENTER  TO W-IDORDNR7-MIN                   
000780          ELSE                                                            
000781            MOVE ZERO                 TO W-IDORDNR7-MIN                   
000782          END-IF                                                          
000783          IF SPAR-IDKOLLI-ENTER NUMERIC                                   
000784            MOVE SPAR-IDKOLLI-ENTER TO W-IDKOLLI-MIN                      
000785          ELSE                                                            
000786            MOVE ZERO               TO W-IDKOLLI-MIN                      
000787          END-IF                                                          
000788                                                                          
000789*---    NYCKLAR FÖR SAMKOLLIN I R-STATUS                                  
000790*---    VI NOLLSTÄLLER FÖR ATT ALLTID FÅ ALLRA ÄLDSTA                     
000791*---    FÖRST (EJ ENTER NYCKEL) VID HOPP FRÅN ANNAN BILD                  
000792                                                                          
000793          MOVE ZERO           TO W-7C1-TIREGDAT                           
000794          MOVE SPACE          TO W-7C1-IDDC                               
000795          MOVE ZERO           TO W-7C1-IDKOLLI-SAMP                       
000796                                                                          
000797       ELSE                                                               
000798         MOVE ZERO            TO W-IDKOLLI-SAMP                           
000799                                 W-IDDISTR-MIN                            
000800                                 W-IDKUNDNR-MIN                           
000801                                 W-IDORDNR7-MIN                           
000802                                 W-IDKOLLI-MIN                            
000803                                 W-7C1-TIREGDAT                           
000804                                 W-7C1-IDKOLLI-SAMP                       
000805        MOVE SPACE            TO W-7C1-IDDC                               
000806                                                                          
000807         PERFORM MFS-RENSA-FAELT-IN                                       
000808       END-IF                                                             
000809     ELSE                                                                 
000810       MOVE ZERO            TO W-IDKOLLI-SAMP                             
000811                               W-IDDISTR-MIN                              
000812                               W-IDKUNDNR-MIN                             
000813                               W-IDORDNR7-MIN                             
000814                               W-IDKOLLI-MIN                              
000815                               W-7C1-TIREGDAT                             
000816                               W-7C1-IDKOLLI-SAMP                         
000817       MOVE SPACE           TO W-7C1-IDDC                                 
000818       PERFORM MFS-RENSA-FAELT-IN                                         
000819     END-IF                                                               
000820     .                                                                    
000821                                                                          
000822 F-LAES-VISA-INFO SECTION.                                                
000823     MOVE 'F-LAES-VISA-INFO '    TO CURRENT-SECTION                       
000824                                                                          
000825     IF SOEK-REOPENED-SK                                                  
000826        PERFORM FC-VISA-REOPENED-SKOLLI                                   
000827     ELSE                                                                 
000828       IF SOEK-INGAENDE-KOLLI                                             
000829          PERFORM FA-VISA-INGAENDE-IDKOLLI                                
000830       ELSE                                                               
000831          PERFORM FB-VISA-IDKOLLI-SAMP                                    
000832       END-IF                                                             
000833     END-IF                                                               
000834     .                                                                    
000835                                                                          
000836 FA-VISA-INGAENDE-IDKOLLI SECTION.                                        
000837     MOVE 'FA-VISA-INGAENDE '    TO CURRENT-SECTION                       
000838                                                                          
000839     PERFORM IMS-GU-WDE711-ASEQ                                           
000840                                                                          
000841     IF SEGMENT-SAKNAS                                                    
000842       MOVE ERR-INFO-MISSING    TO MED-IDMFSFEL                           
000843       CALL WMEDKONV USING MED-WMEDAREA                                   
000844       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000845       PERFORM MFS-RENSA-FAELT-UT                                         
000846       MOVE MID-IDKOLLI-SAMP-IN TO MOD-IDKOLLI-SAMP-UT                    
000847                                   SPAR-IDKOLLI-SAMP-ENTER                
000848       MOVE WS-IDDC-IN          TO MOD-IDDC-UT                            
000849                                   SPAR-IDDC-ENTER                        
000850     ELSE                                                                 
000851       PERFORM IMS-GNP-WDE721-ASEQ-KVAL                                   
000852       IF SEGMENT-FINNS                                                   
000853         PERFORM FAA-SKAPA-ENTER-KEY                                      
000854         MOVE +1 TO INDX                                                  
000855                                                                          
000856         PERFORM UNTIL INDX > MAX-INDX                                    
000857           IF SEGMENT-FINNS                                               
000858             PERFORM FAB-SKAPA-MOD-RADER                                  
000859             ADD +1 TO INDX                                               
000860           ELSE                                                           
000861             MOVE MFS-RENSA-FAELT TO MOD-IDDISTR (INDX)                   
000862                                     MOD-IDKUNDNR (INDX)                  
000863                                     MOD-IDORDNR7 (INDX)                  
000864                                     MOD-IDKOLLI (INDX)                   
000865             ADD +1 TO INDX                                               
000866           END-IF                                                         
000867           IF SEGMENT-FINNS                                               
000868              PERFORM IMS-GNP-WDE721-ASEQ-KVAL                            
000869           END-IF                                                         
000870         END-PERFORM                                                      
000871                                                                          
000872         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP                         
000873                                 MOD-IDDC                                 
000874                                 MOD-IDDISTR-UT                           
000875                                 MOD-IDKUNDNR-UT                          
000876                                 MOD-IDORDNR7-UT                          
000877                                 MOD-IDKOLLI-UT                           
000878                                 MOD-FLREOPENED                           
000879                                                                          
000880         IF SEGMENT-FINNS                                                 
000881            PERFORM FAC-SKAPA-NEXT-KEY                                    
000882            MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                     
000883            CALL WMEDKONV USING MED-WMEDAREA                              
000884            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
000885         ELSE                                                             
000886                                                                          
000887           MOVE ZERO        TO SPAR-IDKOLLI-SAMP-NEXT                     
000888                               SPAR-IDDISTR-NEXT                          
000889                               SPAR-IDKUNDNR-NEXT                         
000890                               SPAR-IDORDNR7-NEXT                         
000891                               SPAR-IDKOLLI-NEXT                          
000892                               SPAR-TIREGDAT-R-NEXT                       
000893                               SPAR-IDKOLLI-SAMP-R-NEXT                   
000894           MOVE SPACE       TO SPAR-IDDC-NEXT                             
000895                               SPAR-IDDC-R-NEXT                           
000896                                                                          
000897           MOVE THIS-IS-THE-LAST-PAGE TO MED-IDMFSINF                     
000898           CALL WMEDKONV USING MED-WMEDAREA                               
000899           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
000900         END-IF                                                           
000901       ELSE                                                               
000902         MOVE 'INGA KOLLIN FINNS I SAMLINGSKOLLIT' TO MOD-TEMFSFEL        
000903         PERFORM MFS-RENSA-FAELT-UT                                       
000904         MOVE MID-IDKOLLI-SAMP-IN TO MOD-IDKOLLI-SAMP-UT                  
000905                                   SPAR-IDKOLLI-SAMP-ENTER                
000906         MOVE WS-IDDC-IN          TO MOD-IDDC-UT                          
000907                                   SPAR-IDDC-ENTER                        
000908       END-IF                                                             
000909                                                                          
000910     END-IF                                                               
000911                                                                          
000912     MOVE '002'                     TO MSGI-KDCALL                        
000913     MOVE MFS-KDMFSFOR              TO MSGI-KDMFSFOR                      
000914     MOVE '4344'                    TO SPAR-IDTRANS                       
000915     MOVE SPAR-AREA                 TO MSGI-SPAR-AREA                     
000916     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000917     .                                                                    
000918     EJECT                                                                
000919 FAA-SKAPA-ENTER-KEY        SECTION.                                      
000920     MOVE 'FAA-SKAPA-ENTER- '    TO CURRENT-SECTION                       
000921                                                                          
000922     IF SEGMENT-FINNS                                                     
000923       MOVE SKLI-IDKOLLI-SAMP    TO SPAR-IDKOLLI-SAMP-ENTER               
000924       MOVE SKLI-IDDC            TO SPAR-IDDC-ENTER                       
000925       MOVE SKOR-IDDISTR         TO SPAR-IDDISTR-ENTER                    
000926       MOVE SKOR-IDKUNDNR        TO SPAR-IDKUNDNR-ENTER                   
000927       MOVE SKOR-IDORDNR7        TO SPAR-IDORDNR7-ENTER                   
000928       MOVE SKOR-IDKOLLI         TO SPAR-IDKOLLI-ENTER                    
000929     END-IF                                                               
000930                                                                          
000931     MOVE ZERO                   TO SPAR-TIREGDAT-R-ENTER                 
000932                                    SPAR-IDKOLLI-SAMP-R-ENTER             
000933     MOVE SPACE                  TO SPAR-IDDC-R-ENTER                     
000934     .                                                                    
000935                                                                          
000936     EJECT                                                                
000937 FAB-SKAPA-MOD-RADER SECTION.                                             
000938     MOVE 'FAB-SKAPA-MOD-RAD'    TO CURRENT-SECTION                       
000939                                                                          
000940     MOVE SKOR-IDDISTR          TO MOD-IDDISTR  (INDX)                    
000941     MOVE SKOR-IDKUNDNR         TO MOD-IDKUNDNR (INDX)                    
000942     MOVE SKOR-IDORDNR7         TO MOD-IDORDNR7 (INDX)                    
000943     MOVE SKOR-IDKOLLI          TO MOD-IDKOLLI  (INDX)                    
000944                                                                          
000945     .                                                                    
000946     EJECT                                                                
000947 FAC-SKAPA-NEXT-KEY        SECTION.                                       
000948     MOVE 'FAC-SKAPA-NEXT-KEY'    TO CURRENT-SECTION                      
000949                                                                          
000950     MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                          
000951     CALL WMEDKONV USING MED-WMEDAREA                                     
000952     MOVE MED-TEMFSINF           TO MOD-TEMFSINF                          
000953                                                                          
000954     MOVE SKLI-IDKOLLI-SAMP      TO SPAR-IDKOLLI-SAMP-NEXT                
000955     MOVE SKLI-IDDC              TO SPAR-IDDC-NEXT                        
000956     MOVE SKOR-IDDISTR           TO SPAR-IDDISTR-NEXT                     
000957     MOVE SKOR-IDKUNDNR          TO SPAR-IDKUNDNR-NEXT                    
000958     MOVE SKOR-IDORDNR7          TO SPAR-IDORDNR7-NEXT                    
000959     MOVE SKOR-IDKOLLI           TO SPAR-IDKOLLI-NEXT                     
000960                                                                          
000961     MOVE ZERO                   TO SPAR-TIREGDAT-R-NEXT                  
000962                                    SPAR-IDKOLLI-SAMP-R-NEXT              
000963     MOVE SPACE                  TO SPAR-IDDC-R-NEXT                      
000964                                                                          
000965     .                                                                    
000966     EJECT                                                                
000967                                                                          
000968 FB-VISA-IDKOLLI-SAMP     SECTION.                                        
000969     MOVE 'FB-VISA-IDKOLLI-SAMP'    TO CURRENT-SECTION                    
000970                                                                          
000971*    SÖKER MAN PÅ ETT SK, FINNS INGEN BLÄDDRING                           
000972                                                                          
000973     MOVE ZERO                      TO SPAR-IDKOLLI-SAMP-NEXT             
000974                                       SPAR-IDDISTR-NEXT                  
000975                                       SPAR-IDKUNDNR-NEXT                 
000976                                       SPAR-IDORDNR7-NEXT                 
000977                                       SPAR-IDKOLLI-NEXT                  
000978                                       SPAR-TIREGDAT-R-NEXT               
000979                                       SPAR-IDKOLLI-SAMP-R-NEXT           
000980     MOVE SPACE                     TO SPAR-IDDC-NEXT                     
000981                                       SPAR-IDDC-R-NEXT                   
000982                                                                          
000983     MOVE ZERO                      TO SPAR-IDKOLLI-SAMP-ENTER            
000984                                       SPAR-IDDISTR-ENTER                 
000985                                       SPAR-IDKUNDNR-ENTER                
000986                                       SPAR-IDORDNR7-ENTER                
000987                                       SPAR-IDKOLLI-ENTER                 
000988                                       SPAR-TIREGDAT-R-ENTER              
000989                                       SPAR-IDKOLLI-SAMP-R-ENTER          
000990     MOVE SPACE                     TO SPAR-IDDC-ENTER                    
000991                                       SPAR-IDDC-R-ENTER                  
000992                                                                          
000993     PERFORM IMS-GU-WDE7B1                                                
000994                                                                          
000995     IF SEGMENT-SAKNAS                                                    
000996        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
000997        CALL WMEDKONV USING MED-WMEDAREA                                  
000998        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
000999        PERFORM MFS-RENSA-FAELT-UT                                        
001000        MOVE MID-IDDISTR-IN  TO MOD-IDDISTR-UT                            
001001        MOVE MID-IDKUNDNR-IN TO MOD-IDKUNDNR-UT                           
001002        MOVE MID-IDORDNR7-IN TO MOD-IDORDNR7-UT                           
001003        MOVE MID-IDKOLLI-IN  TO MOD-IDKOLLI-UT                            
001004                                                                          
001005     ELSE                                                                 
001006       PERFORM FBB-SKAPA-MOD-SKOLLI-RAD                                   
001007       MOVE MFS-RENSA-FAELT  TO MOD-IDKOLLI-SAMP-UT                       
001008                                MOD-IDDC-UT                               
001009     END-IF                                                               
001010                                                                          
001011*--- RENSA D-K-O-K TABELL I BILDEN, INTE RELEVANT HÄR                     
001012     MOVE +1 TO INDX                                                      
001013     PERFORM UNTIL INDX > MAX-INDX                                        
001014       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
001015         ADD +1 TO INDX                                                   
001016     END-PERFORM                                                          
001017                                                                          
001018     MOVE '002'                     TO MSGI-KDCALL                        
001019     MOVE MFS-KDMFSFOR              TO MSGI-KDMFSFOR                      
001020     MOVE '4344'                    TO SPAR-IDTRANS                       
001021     MOVE SPAR-AREA                 TO MSGI-SPAR-AREA                     
001022     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
001023     .                                                                    
001024                                                                          
001025     EJECT                                                                
001026 FBB-SKAPA-MOD-SKOLLI-RAD SECTION.                                        
001027     MOVE 'FBB-SKAPA-MOD-SKOLLI'    TO CURRENT-SECTION                    
001028                                                                          
001029     MOVE SEQB-IDKOLLI-SAMP     TO MOD-IDKOLLI-SAMP                       
001030     MOVE SEQB-IDDC             TO MOD-IDDC                               
001031     .                                                                    
001032                                                                          
001033                                                                          
001034 FC-VISA-REOPENED-SKOLLI SECTION.                                         
001035     MOVE 'FC-VISA-REOPENED '    TO CURRENT-SECTION                       
001036                                                                          
001037*--- LÄS ALLA SK I R-STATUS                                               
001038                                                                          
001039     PERFORM IMS-GN-WDE7C1-KVAL                                           
001040                                                                          
001041     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
001042       MOVE 'INGA ÅTERÖPPNADE SAMLINGSKOLLIN FINNS'                       
001043                                 TO MOD-TEMFSFEL                          
001044       PERFORM MFS-RENSA-FAELT-UT                                         
001045                                                                          
001046       MOVE ZERO                    TO SPAR-IDKOLLI-SAMP-ENTER            
001047                                       SPAR-IDDISTR-ENTER                 
001048                                       SPAR-IDKUNDNR-ENTER                
001049                                       SPAR-IDORDNR7-ENTER                
001050                                       SPAR-IDKOLLI-ENTER                 
001051                                       SPAR-TIREGDAT-R-ENTER              
001052                                       SPAR-IDKOLLI-SAMP-R-ENTER          
001053       MOVE SPACE                   TO SPAR-IDDC-ENTER                    
001054                                       SPAR-IDDC-R-ENTER                  
001055                                                                          
001056     ELSE                                                                 
001057       PERFORM FCA-SKAPA-ENTER-KEY                                        
001058       MOVE +1 TO INDX-R                                                  
001059                                                                          
001060       PERFORM UNTIL INDX-R > MAX-INDX-R                                  
001061         IF SEGMENT-FINNS                                                 
001062           PERFORM FCB-SKAPA-MOD-RADER                                    
001063         ELSE                                                             
001064           MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP-R (INDX-R)            
001065                                   MOD-IDDC-R         (INDX-R)            
001066                                   MOD-TIREGDAT-R     (INDX-R)            
001067         END-IF                                                           
001068         IF SEGMENT-FINNS                                                 
001069            PERFORM IMS-GN-WDE7C1-KVAL                                    
001070         END-IF                                                           
001071         ADD +1 TO INDX-R                                                 
001072       END-PERFORM                                                        
001073                                                                          
001074       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP                           
001075                               MOD-IDDC                                   
001076                               MOD-IDDISTR-UT                             
001077                               MOD-IDKUNDNR-UT                            
001078                               MOD-IDORDNR7-UT                            
001079                               MOD-IDKOLLI-UT                             
001080                               MOD-IDKOLLI-SAMP-UT                        
001081                               MOD-IDDC-UT                                
001082                                                                          
001083       IF SEGMENT-FINNS                                                   
001084          PERFORM FCC-SKAPA-NEXT-KEY                                      
001085          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
001086          CALL WMEDKONV USING MED-WMEDAREA                                
001087          MOVE MED-TEMFSINF TO MOD-TEMFSINF                               
001088       ELSE                                                               
001089                                                                          
001090*LLB EV FLYTTA R-ENTER TILL R-NEXT VID BLÄDDRING EFTER SISTA SIDAN        
001091         MOVE ZERO        TO SPAR-IDKOLLI-SAMP-NEXT                       
001092                             SPAR-IDKOLLI-SAMP-R-NEXT                     
001093                             SPAR-IDDISTR-NEXT                            
001094                             SPAR-IDKUNDNR-NEXT                           
001095                             SPAR-IDORDNR7-NEXT                           
001096                             SPAR-IDKOLLI-NEXT                            
001097                             SPAR-TIREGDAT-R-NEXT                         
001098                             SPAR-IDKOLLI-SAMP-R-NEXT                     
001099         MOVE SPACE       TO SPAR-IDDC-NEXT                               
001100                             SPAR-IDDC-R-NEXT                             
001101                                                                          
001102         MOVE THIS-IS-THE-LAST-PAGE TO MED-IDMFSINF                       
001103         CALL WMEDKONV USING MED-WMEDAREA                                 
001104         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
001105       END-IF                                                             
001106     END-IF                                                               
001107                                                                          
001108*--- RENSA D-K-O-K TABELL I BILDEN, INTE RELEVANT HÄR                     
001109     MOVE +1 TO INDX                                                      
001110     PERFORM UNTIL INDX > MAX-INDX                                        
001111       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
001112         ADD +1 TO INDX                                                   
001113     END-PERFORM                                                          
001114                                                                          
001115     MOVE '002'                     TO MSGI-KDCALL                        
001116     MOVE MFS-KDMFSFOR              TO MSGI-KDMFSFOR                      
001117     MOVE '4344'                    TO SPAR-IDTRANS                       
001118     MOVE SPAR-AREA                 TO MSGI-SPAR-AREA                     
001119     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
001120     .                                                                    
001121     EJECT                                                                
001122 FCA-SKAPA-ENTER-KEY        SECTION.                                      
001123     MOVE 'FAA-SKAPA-ENTER- '    TO CURRENT-SECTION                       
001124                                                                          
001125     MOVE ZERO                   TO SPAR-IDKOLLI-SAMP-ENTER               
001126                                    SPAR-IDDISTR-ENTER                    
001127                                    SPAR-IDKUNDNR-ENTER                   
001128                                    SPAR-IDORDNR7-ENTER                   
001129                                    SPAR-IDKOLLI-ENTER                    
001130                                                                          
001131     MOVE SPACE                  TO SPAR-IDDC-ENTER                       
001132                                                                          
001133     MOVE SEQC-TIREGDAT          TO SPAR-TIREGDAT-R-ENTER                 
001134     MOVE SEQC-IDDC              TO SPAR-IDDC-R-ENTER                     
001135     MOVE SEQC-IDKOLLI-SAMP      TO SPAR-IDKOLLI-SAMP-R-ENTER             
001136     .                                                                    
001137                                                                          
001138     EJECT                                                                
001139 FCB-SKAPA-MOD-RADER SECTION.                                             
001140     MOVE 'FCB-SKAPA-MOD-RAD'    TO CURRENT-SECTION                       
001141                                                                          
001142     MOVE SEQC-IDKOLLI-SAMP     TO MOD-IDKOLLI-SAMP-R (INDX-R)            
001143     MOVE SEQC-IDDC             TO MOD-IDDC-R         (INDX-R)            
001144     MOVE SEQC-TIREGDAT         TO MOD-TIREGDAT-R     (INDX-R)            
001145                                                                          
001146     .                                                                    
001147     EJECT                                                                
001148 FCC-SKAPA-NEXT-KEY        SECTION.                                       
001149     MOVE 'FCC-SKAPA-NEXT-KEY'    TO CURRENT-SECTION                      
001150                                                                          
001151     MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                          
001152     CALL WMEDKONV USING MED-WMEDAREA                                     
001153     MOVE MED-TEMFSINF           TO MOD-TEMFSINF                          
001154                                                                          
001155     MOVE SPACE                  TO SPAR-IDDC-NEXT                        
001156     MOVE ZERO                   TO SPAR-IDKOLLI-SAMP-NEXT                
001157                                    SPAR-IDDISTR-NEXT                     
001158                                    SPAR-IDKUNDNR-NEXT                    
001159                                    SPAR-IDORDNR7-NEXT                    
001160                                    SPAR-IDKOLLI-NEXT                     
001161                                                                          
001162     MOVE SEQC-TIREGDAT          TO SPAR-TIREGDAT-R-NEXT                  
001163     MOVE SEQC-IDDC              TO SPAR-IDDC-R-NEXT                      
001164     MOVE SEQC-IDKOLLI-SAMP      TO SPAR-IDKOLLI-SAMP-R-NEXT              
001165                                                                          
001166     .                                                                    
001167     EJECT                                                                
001168                                                                          
001169                                                                          
001170 MFS-RENSA-FAELT-UT SECTION.                                              
001171     MOVE 'MFS-RENSA-FAELT-UT  '    TO CURRENT-SECTION                    
001180                                                                          
001190     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP                             
001200                             MOD-IDDC                                     
001210                                                                          
001211     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
001212                             MOD-IDKUNDNR-UT                              
001213                             MOD-IDORDNR7-UT                              
001214                             MOD-IDKOLLI-UT                               
001215                             MOD-IDKOLLI-SAMP-UT                          
001216                             MOD-IDDC-UT                                  
001217                                                                          
001218*--- RENSA 'INGÅENDE KOLLIN'-TABELL                                       
001219     MOVE +1 TO INDX                                                      
001220     PERFORM UNTIL INDX > MAX-INDX                                        
001221       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
001222       ADD +1 TO INDX                                                     
001223     END-PERFORM                                                          
001224                                                                          
001225*--- RENSA 'R-STATUS SAMKOLLIN'-TABELL                                    
001226     MOVE +1 TO INDX-R                                                    
001227     PERFORM UNTIL INDX-R > MAX-INDX-R                                    
001228       PERFORM MFS-RENSA-RAD-FAELT-R-UT                                   
001229       ADD +1 TO INDX-R                                                   
001230     END-PERFORM                                                          
001231     .                                                                    
001232                                                                          
001233 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
001234                                                                          
001235*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
001236                                                                          
001237     MOVE MFS-RENSA-FAELT TO        MOD-IDDISTR  (INDX)                   
001238                                    MOD-IDKUNDNR (INDX)                   
001239                                    MOD-IDORDNR7 (INDX)                   
001240                                    MOD-IDKOLLI  (INDX)                   
001241     .                                                                    
001242                                                                          
001243 MFS-RENSA-RAD-FAELT-R-UT SECTION.                                        
001244                                                                          
001245*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
001246                                                                          
001247     MOVE MFS-RENSA-FAELT TO        MOD-TIREGDAT-R     (INDX-R)           
001248                                    MOD-IDDC-R         (INDX-R)           
001249                                    MOD-IDKOLLI-SAMP-R (INDX-R)           
001250     .                                                                    
001251                                                                          
001252 MFS-RENSA-FAELT-IN SECTION.                                              
001253                                                                          
001254*    --- ALLA INDATA-FÄLT                                                 
001255                                                                          
001256     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
001257                             MOD-IDKUNDNR-IN                              
001258                             MOD-IDORDNR7-IN                              
001259                             MOD-IDKOLLI-IN                               
001260                             MOD-IDKOLLI-SAMP-IN                          
001261                             MOD-IDDC-IN                                  
001262                                                                          
001263     .                                                                    
001264                                                                          
001265     EJECT                                                                
001266 IMS-GET-MSG SECTION.                                                     
001267                                                                          
001268     MOVE '  QC' TO GODK-STATUSKODER                                      
001269     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001270     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001271     PERFORM IMS-STATUSKONTROLL                                           
001272     .                                                                    
001273                                                                          
001274 IMS-INSERT-MSG SECTION.                                                  
001275                                                                          
001276     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
001277       MOVE '0' TO MFS-KDHUVOMR                                           
001278     END-IF                                                               
001279     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
001280     MOVE SPACE TO GODK-STATUSKODER                                       
001281     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
001282     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001283     PERFORM IMS-STATUSKONTROLL                                           
001284     .                                                                    
001285                                                                          
001286     EJECT                                                                
001287                                                                          
001288 IMS-GU-WDE711-ASEQ  SECTION.                                             
001289                                                                          
001290     MOVE 'IMS-GU-WDE711-ASEQ '    TO CURRENT-IMS-SECTION                 
001291                                                                          
001292     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
001293          DELIMITED BY SIZE INTO SSA1                                     
001294                                                                          
001295     MOVE '  GE' TO GODK-STATUSKODER                                      
001296     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE711 SSA1                    
001297     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
001298                                                                          
001299     PERFORM IMS-STATUSKONTROLL                                           
001300     .                                                                    
001301                                                                          
001302 IMS-GNP-WDE721-ASEQ-KVAL  SECTION.                                       
001303     MOVE 'IMS-GNP-WDE721-ASEQ-KVAL '    TO CURRENT-IMS-SECTION           
001304                                                                          
001305*    STARTA LÄSN I SEKV FRÅN START ELLER                                  
001306*    FORTSÄTT LÄSNING FRÅN SENAST VISADE VID PF8                          
001307                                                                          
001308     STRING 'WDE721  (WDE721KY>=' W-WDE721KY-MIN-X                        
001309                    '&WDE721KY<=' W-WDE721KY-MAX-X ')'                    
001310          DELIMITED BY SIZE INTO SSA1                                     
001311                                                                          
001312     MOVE '  GE'                 TO GODK-STATUSKODER                      
001313     CALL CBLTDLI USING GNP  WDE7-PCB DLI-IO-WDE721 SSA1                  
001314     MOVE WDE7-STATUS-CODE      TO STATUS-WS                              
001315                                                                          
001316     PERFORM IMS-STATUSKONTROLL                                           
001317     .                                                                    
001318     EJECT                                                                
001319                                                                          
001320 IMS-GN-WDE7C1-KVAL  SECTION.                                             
001321                                                                          
001322*    EN GN SOM LÄSER ALLA WDE7C1-SEGMENT DVS                              
001323*    ALLA SK I R-STATUS OAVSETT DC                                        
001324*    DELS FRÅN START , DELS FRÅN VISST SK (VID BLÄDDRING). .              
001325                                                                          
001326     MOVE 'IMS-GN-WDE7C1      '    TO CURRENT-IMS-SECTION                 
001327                                                                          
001328     STRING 'WDE7C1  (WDE7C1KY>=' W-WDE7C1KY-X ')'                        
001329          DELIMITED BY SIZE INTO SSA1                                     
001330     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001331                                                                          
001332     CALL CBLTDLI USING GN WDE7C-PCB DLI-IO-WDE7C1 SSA1                   
001333     MOVE WDE7C-STATUS-CODE TO STATUS-WS                                  
001334     PERFORM IMS-STATUSKONTROLL                                           
001335                                                                          
001336     .                                                                    
001337     EJECT                                                                
001338                                                                          
001339 IMS-GU-WDE7B1       SECTION.                                             
001340                                                                          
001341     MOVE 'IMS-GU-WDE7B1      '    TO CURRENT-IMS-SECTION                 
001342                                                                          
001343     STRING 'WDE7B1  (WDE7B1KY =' W-WDE7B1KY-X ')'                        
001344          DELIMITED BY SIZE INTO SSA1                                     
001345     MOVE '  GE' TO GODK-STATUSKODER                                      
001346     CALL CBLTDLI USING GU WDE7B-PCB DLI-IO-WDE7B1 SSA1                   
001347     MOVE WDE7B-STATUS-CODE TO STATUS-WS                                  
001348     PERFORM IMS-STATUSKONTROLL                                           
001349     .                                                                    
001350     EJECT                                                                
001351 IMS-STATUSKONTROLL SECTION.                                              
001352                                                                          
001353     SET STATUS-IX TO 1                                                   
001354     SEARCH GODK-STATUS                                                   
001355       AT END                                                             
001356         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001357         DELIMITED BY SIZE INTO FELTEXT                                   
001358         CALL FELLOG                                                      
001359       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001360         CONTINUE                                                         
001361     END-SEARCH                                                           
001362     .                                                                    
