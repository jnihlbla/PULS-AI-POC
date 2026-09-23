000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W3717800.                                                
000004 AUTHOR.         RONNY STENHOLM.                                          
000005 DATE-WRITTEN.   96/10/16.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008                                                                          
000009*    FUNKTION:                                                            
000010*        SKAPA FAKTUROR FÖR UTLEV AV OBJ TILL SITTARD OCH ENGLAND         
000011*                                                                         
000012*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
000013*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
000014*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
000015*        PROGRAMMET UPPDATERAR WL3165 (WDGX)                              
000016*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
000017*        PROGRAMMET UPPDATERAR        (WDR8)                              
000018*                                                                         
000019*    ABENDKODER:                                                          
000020*        U0016 -  . . . .                                                 
000021*        U1000 -  . . . .                                                 
000022*                                                                         
000023* ETRACKER 5594753/ 071023/EÖ STOPPA URSKRIFT PRINTER SITTARD             
000024* ETRACKER 10228590/ 140428/HB SKAPA CORE PROFORMA FÖR CANADA             
000025*                           SOM WEB DOKUMENT (DOCUMENT RETRIVAL).         
000026*                                                                         
000027                                                                          
000028     SKIP3                                                                
000029 ENVIRONMENT DIVISION.                                                    
000030     SKIP2                                                                
000031 INPUT-OUTPUT SECTION.                                                    
000032                                                                          
000033 FILE-CONTROL.                                                            
000034     SKIP2                                                                
000035*          --- FIL MED RAPPORTER TILL WDM6                                
000036     SELECT W3717H                     ASSIGN TO W37178D2.                
000037     SELECT W37178                     ASSIGN TO W37178D3.                
000038     SELECT W3717C                     ASSIGN TO W37178D4.                
000039     EJECT                                                                
000040 DATA DIVISION.                                                           
000041     SKIP3                                                                
000042 FILE SECTION.                                                            
000043     SKIP3                                                                
000044 FD  W3717H                                                               
000045     RECORDING       F                                                    
000046     BLOCK CONTAINS  0.                                                   
000047                                                                          
000048*01  POST -COPY W37116 -PRE  BYTES-  -L.                                  
000049     EJECT                                                                
000050 FD  W37178                                                               
000051     RECORDING       V                                                    
000052     BLOCK CONTAINS  0.                                                   
000053                                                                          
000054 01  UTPOST                      PIC X(135).                              
000055     EJECT                                                                
000056 FD  W3717C                                                               
000057     RECORDING       V                                                    
000058     BLOCK CONTAINS  0.                                                   
000059                                                                          
000060 01  UTPOST2                     PIC X(134).                              
000061     EJECT                                                                
000062 WORKING-STORAGE SECTION.                                                 
000063                                                                          
000064 77  IDPGM                      PIC X(8)    VALUE 'W3717800'.             
000065 77  JA                         PIC X       VALUE 'J'.                    
000066 77  NEJ                        PIC X       VALUE 'N'.                    
000067 77  SKRIV-BYTES-RAPPORT        PIC X       VALUE 'N'.                    
000068 77  SPAR-IDDISTR               PIC S9(5) VALUE ZERO COMP-3.              
000069 77  WS-VKART                   PIC S9(7) VALUE ZERO COMP-3.              
000070 77  WS-VKART-UNIT              PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000071 77  WS-VKART-TOT               PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000072 77  WS-SUVKART-TOT             PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000073 77  WS-KVLEVART-TOT            PIC S9(7) VALUE ZERO COMP-3.              
000074 77  WS-IDBYTKOL-TOT            PIC S9(6) VALUE ZERO COMP-3.              
000075 77  WS-VOLYM                   PIC S9(6)V9(1) VALUE ZERO.                
000076 77  WS-PRAVCOST                PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000077                                                                          
000078 77  WS-SUFKTUTL-TOT            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000079 77  WS-PRFKTUTL-RAD            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000080 77  WS-PRFKTUTL                PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000081 77  INDX                       PIC S9(2) COMP SYNC.                      
000082 77  WS-ANTAL-RADER             PIC S9(4)      VALUE ZERO.                
000083 77  WS-SIDA                    PIC S9(2)      VALUE ZERO.                
000084 77  WS-SIDA-MAX                PIC S9(2)      VALUE ZERO.                
000085 77  WS-REST                    PIC S9(2)      VALUE ZERO.                
000086 77  WS-DUMMY                   PIC S9(2)      VALUE ZERO.                
000087 77  W-DATE-AAMM                PIC 9(4)       VALUE ZERO.                
000088 77  WS-KDVALISO-HUV            PIC X(3)       VALUE 'SEK'.               
000089                                                                          
000090 77  WS-VKORDBTO-UTAN-DEC       PIC 9(6)       VALUE ZERO.                
000091 77  WS-VLORDBTO-UTAN-DEC       PIC 9(6)       VALUE ZERO.                
000092                                                                          
000093 77  WS-PRKURS                  PIC S9(6)V9(5) VALUE ZERO COMP-3.         
000094                                                                          
000095 77  SPAR-IDBYTKOL              PIC 9(3) VALUE ZERO.                      
000096                                                                          
000097 01  WS-IDBYTRAP-2               PIC 9(7) COMP-3.                         
000098                                                                          
000099 01  WS-IDBYTRAP                 PIC 9(7) VALUE ZERO.                     
000100 01  FILLER REDEFINES WS-IDBYTRAP.                                        
000101     03 WS-IDBYTFAK              PIC X(4).                                
000102     03 WS-IDBYTKOL              PIC X(3).                                
000103                                                                          
000104 01  PRT-AREA.                                                            
000105     03 WS-RAD                   PIC X(130).                              
000106     SKIP2                                                                
000107 77  SKRIV-RUBRIK-SW             PIC X       VALUE 'N'.                   
000108     88  SKRIV-RUBRIK                        VALUE 'J'.                   
000109     88  SKRIV-EJ-RUBRIK                     VALUE 'N'.                   
000110 77  FAKTURA-HITTAD-SW           PIC X       VALUE 'N'.                   
000111     88  FAKTURA-HITTAD                      VALUE 'J'.                   
000112     88  SAKNAS                              VALUE 'N'.                   
000113 77  FAKTURAN-SLUT-SW            PIC X       VALUE 'N'.                   
000114     88  FAKTURAN-SLUT                       VALUE 'J'.                   
000115     88  FAKTURAN-KVAR                       VALUE 'N'.                   
000116 77  FL-WEBDC-SW                 PIC X       VALUE 'N'.                   
000117     88  FL-WEBDC                            VALUE 'J'.                   
000118     88  FL-CLASSIC                          VALUE 'N'.                   
000119 01  FELTEXT.                                                             
000120     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000121     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000122     EJECT                                                                
000123 01  DYNAMISKA-SUBPROGRAM.                                                
000124*                                                                         
000125     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000126     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000127     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000128     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000129     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
000130     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
000131     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
000132     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000133     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000134     EJECT                                                                
000135*    --- PARAMETRAR TILL POSTSUM                                          
000136*                                                                         
000137*01  -COPY W0005   -PRE  POSTSUM-                                         
000138     EJECT                                                                
000139*    --- PARAMETRAR TILL W009CIA                                          
000140*                                                                         
000141*01  -COPY W009CIA                                                        
000142     EJECT                                                                
000143*    --- PARAMETRAR TILL W510CURR                                         
000144*                                                                         
000145*01  -COPY W510CURR                                                       
000146     EJECT                                                                
000147*    -- VALID IDDC CODES                                                  
000148*                                                                         
000149*01  -COPY WWDC99                                                         
000150*01  -COPY WWDCKONS                                                       
000151     EJECT                                                                
000152 01  FILLER                  PIC X(16) VALUE 'PRISTILL-AREA'.             
000153*01  PRIS-AREA  -COPY W335PRIS                                            
000154     EJECT                                                                
000155*- - - - - - - - - - - - - -  PARAMETRAR TILL W400ARTU                    
000156*01  -COPY W400ARTU                                                       
000157     SKIP2                                                                
000158 01  RAD                         PIC X(130).                              
000159     SKIP2                                                                
000160*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
000161 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000162     SKIP2                                                                
000163*01  -COPY WDATKORT                                                       
000164     EJECT                                                                
000165 01  UT-AREA-START              PIC X(24)   VALUE                         
000166                                'UT-AREA-START  '.                        
000167     SKIP2                                                                
000168 01  UT-RAD                     PIC X(130)  VALUE SPACE.                  
000169     EJECT                                                                
000170                                                                          
000171*    --- LISTAN                                                           
000172 01  RUBRIK-RAD.                                                          
000173     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
000174     03  FILLER            PIC X(1)  VALUE SPACE.                         
000175     03  FILLER            PIC X(8)  VALUE 'EXCHANGE'.                    
000176     03  FILLER            PIC X(1)  VALUE SPACE.                         
000177     03  FILLER            PIC X(16) VALUE 'PROFORMA INVOICE'.            
000178     03  FILLER            PIC X(5)  VALUE SPACE.                         
000179     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
000180     03  LIST-MAN          PIC X(2).                                      
000181     03  FILLER            PIC X(1) VALUE '/'.                            
000182     03  LIST-DAG          PIC X(2).                                      
000183     03  FILLER            PIC X(1) VALUE '/'.                            
000184     03  LIST-AAR          PIC X(4).                                      
000185     03  FILLER            PIC X(11) VALUE SPACE.                         
000186     03  FILLER            PIC X(15) VALUE 'W37178-001'.                  
000187* ANTAL BYTES                                                             
000188                                                                          
000189 01  E-RUBRIK-RAD.                                                        
000190     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
000191     03  FILLER            PIC X(1)  VALUE ';'.                           
000192     03  FILLER            PIC X(8)  VALUE 'EXCHANGE'.                    
000193     03  FILLER            PIC X(1)  VALUE SPACE.                         
000194     03  FILLER            PIC X(16) VALUE 'PROFORMA INVOICE'.            
000195     03  FILLER            PIC X(1)  VALUE ';'.                           
000196     03  FILLER            PIC X(1)  VALUE ';'.                           
000197     03  FILLER            PIC X(1)  VALUE ';'.                           
000198     03  FILLER            PIC X(1)  VALUE ';'.                           
000199     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
000200     03  FILLER            PIC X(1)  VALUE ';'.                           
000201     03  E-LIST-MAN        PIC X(2).                                      
000202     03  FILLER            PIC X(1) VALUE '/'.                            
000203     03  E-LIST-DAG        PIC X(2).                                      
000204     03  FILLER            PIC X(1) VALUE '/'.                            
000205     03  E-LIST-AAR        PIC X(4).                                      
000206     03  FILLER            PIC X(1)  VALUE ';'.                           
000207     03  FILLER            PIC X(15) VALUE 'W37178-001'.                  
000208     03  FILLER            PIC X(1)  VALUE ';'.                           
000209     03  FILLER            PIC X(1)  VALUE ';'.                           
000210* ANTAL BYTES                                                             
000211                                                                          
000212 01  U-RUBRIK-RAD1.                                                       
000213     03  FILLER            PIC X(10) VALUE 'SHIPPER   '.                  
000214     03  FILLER            PIC X(34) VALUE SPACE.                         
000215     03  FILLER            PIC X(9) VALUE 'CONSIGNEE'.                    
000216     03  FILLER            PIC X(6) VALUE SPACE.                          
000217     03  FILLER            PIC X(5) VALUE 'PAGE'.                         
000218     03  LIST-SIDA         PIC Z(1)9 VALUE ZERO.                          
000219     03  FILLER            PIC X(1) VALUE SPACE.                          
000220     03  FILLER            PIC X(2) VALUE 'OF'.                           
000221     03  FILLER            PIC X(1) VALUE SPACE.                          
000222     03  LIST-SIDA-MAX     PIC Z(1)9 VALUE ZERO.                          
000223* ANTAL BYTES                                                             
000224                                                                          
000225 01  E-U-RUBRIK-RAD1.                                                     
000226     03  FILLER            PIC X(10) VALUE 'SHIPPER   '.                  
000227     03  FILLER            PIC X(1)  VALUE ';'.                           
000228     03  FILLER            PIC X(1)  VALUE ';'.                           
000229     03  FILLER            PIC X(1)  VALUE ';'.                           
000230     03  FILLER            PIC X(1)  VALUE ';'.                           
000231     03  FILLER            PIC X(9)  VALUE 'CONSIGNEE'.                   
000232     03  FILLER            PIC X(1)  VALUE ';'.                           
000233     03  FILLER            PIC X(1)  VALUE ';'.                           
000234     03  FILLER            PIC X(1)  VALUE ';'.                           
000235     03  FILLER            PIC X(1)  VALUE ';'.                           
000236     03  FILLER            PIC X(1)  VALUE ';'.                           
000237* ANTAL BYTES                                                             
000238                                                                          
000239 01  U-RUBRIK-RAD2.                                                       
000240     03  LIST-BEAVS-RAD1 PIC X(35)    VALUE SPACE.                        
000241     03  FILLER          PIC X(9)     VALUE SPACE.                        
000242     03  LIST-BEGMT-RAD1 PIC X(35)    VALUE SPACE.                        
000243* ANTAL BYTES                 116                                         
000244                                                                          
000245 01  E-U-RUBRIK-RAD2.                                                     
000246     03  E-LIST-BEAVS-RAD1 PIC X(35) VALUE SPACE.                         
000247     03  FILLER            PIC X(1)  VALUE ';'.                           
000248     03  FILLER            PIC X(1)  VALUE ';'.                           
000249     03  FILLER            PIC X(1)  VALUE ';'.                           
000250     03  FILLER            PIC X(1)  VALUE ';'.                           
000251     03  E-LIST-BEGMT-RAD1 PIC X(35) VALUE SPACE.                         
000252     03  FILLER            PIC X(1)  VALUE ';'.                           
000253     03  FILLER            PIC X(1)  VALUE ';'.                           
000254     03  FILLER            PIC X(1)  VALUE ';'.                           
000255     03  FILLER            PIC X(1)  VALUE ';'.                           
000256     03  FILLER            PIC X(1)  VALUE ';'.                           
000257* ANTAL BYTES                 116                                         
000258                                                                          
000259 01  U-RUBRIK-RAD3.                                                       
000260     03  LIST-BEAVS-RAD2   PIC X(35)  VALUE SPACE.                        
000261     03  FILLER            PIC X(9)   VALUE SPACE.                        
000262     03  LIST-BEGMT-RAD2   PIC X(35)  VALUE SPACE.                        
000263                                                                          
000264 01  E-U-RUBRIK-RAD3.                                                     
000265     03  E-LIST-BEAVS-RAD2   PIC X(35) VALUE SPACE.                       
000266     03  FILLER              PIC X(1)  VALUE ';'.                         
000267     03  FILLER              PIC X(1)  VALUE ';'.                         
000268     03  FILLER              PIC X(1)  VALUE ';'.                         
000269     03  FILLER              PIC X(1)  VALUE ';'.                         
000270     03  E-LIST-BEGMT-RAD2   PIC X(35) VALUE SPACE.                       
000271     03  FILLER              PIC X(1)  VALUE ';'.                         
000272     03  FILLER              PIC X(1)  VALUE ';'.                         
000273     03  FILLER              PIC X(1)  VALUE ';'.                         
000274     03  FILLER              PIC X(1)  VALUE ';'.                         
000275     03  FILLER              PIC X(1)  VALUE ';'.                         
000276                                                                          
000277 01  U-RUBRIK-RAD4.                                                       
000278     03  LIST-ADAVS-GATA   PIC X(35)  VALUE SPACE.                        
000279     03  FILLER            PIC X(9)   VALUE SPACE.                        
000280     03  LIST-ADGMT-GATA   PIC X(35)  VALUE SPACE.                        
000281     03  FILLER            PIC X(1)   VALUE SPACE.                        
000282                                                                          
000283 01  E-U-RUBRIK-RAD4.                                                     
000284     03  E-LIST-ADAVS-GATA   PIC X(35) VALUE SPACE.                       
000285     03  FILLER              PIC X(1)  VALUE ';'.                         
000286     03  FILLER              PIC X(1)  VALUE ';'.                         
000287     03  FILLER              PIC X(1)  VALUE ';'.                         
000288     03  FILLER              PIC X(1)  VALUE ';'.                         
000289     03  E-LIST-ADGMT-GATA   PIC X(35) VALUE SPACE.                       
000290     03  FILLER              PIC X(1)  VALUE ';'.                         
000291     03  FILLER              PIC X(1)  VALUE ';'.                         
000292     03  FILLER              PIC X(1)  VALUE ';'.                         
000293     03  FILLER              PIC X(1)  VALUE ';'.                         
000294     03  FILLER              PIC X(1)  VALUE ';'.                         
000295                                                                          
000296 01  U-RUBRIK-RAD5.                                                       
000297     03  LIST-ADAVS-PADR   PIC X(35) VALUE SPACE.                         
000298     03  FILLER            PIC X(9)  VALUE SPACE.                         
000299     03  LIST-ADGMT-PADR   PIC X(35) VALUE SPACE.                         
000300     03  FILLER            PIC X(36) VALUE SPACE.                         
000301                                                                          
000302 01  E-U-RUBRIK-RAD5.                                                     
000303     03  E-LIST-ADAVS-PADR   PIC X(35) VALUE SPACE.                       
000304     03  FILLER              PIC X(1)  VALUE ';'.                         
000305     03  FILLER              PIC X(1)  VALUE ';'.                         
000306     03  FILLER              PIC X(1)  VALUE ';'.                         
000307     03  FILLER              PIC X(1)  VALUE ';'.                         
000308     03  E-LIST-ADGMT-PADR   PIC X(35) VALUE SPACE.                       
000309     03  FILLER              PIC X(1)  VALUE ';'.                         
000310     03  FILLER              PIC X(1)  VALUE ';'.                         
000311     03  FILLER              PIC X(1)  VALUE ';'.                         
000312     03  FILLER              PIC X(1)  VALUE ';'.                         
000313     03  FILLER              PIC X(1)  VALUE ';'.                         
000314                                                                          
000315 01  U-RUBRIK-RAD6.                                                       
000316     03  LIST-ADAVS-LAND   PIC X(35) VALUE SPACE.                         
000317     03  FILLER            PIC X(9)  VALUE SPACE.                         
000318     03  LIST-ADGMT-LAND   PIC X(35) VALUE SPACE.                         
000319     03  FILLER            PIC X(36) VALUE SPACE.                         
000320                                                                          
000321 01  E-U-RUBRIK-RAD6.                                                     
000322     03  E-LIST-ADAVS-LAND   PIC X(35) VALUE SPACE.                       
000323     03  FILLER              PIC X(1)  VALUE ';'.                         
000324     03  FILLER              PIC X(1)  VALUE ';'.                         
000325     03  FILLER              PIC X(1)  VALUE ';'.                         
000326     03  FILLER              PIC X(1)  VALUE ';'.                         
000327     03  E-LIST-ADGMT-LAND   PIC X(35) VALUE SPACE.                       
000328     03  FILLER              PIC X(1)  VALUE ';'.                         
000329     03  FILLER              PIC X(1)  VALUE ';'.                         
000330     03  FILLER              PIC X(1)  VALUE ';'.                         
000331     03  FILLER              PIC X(1)  VALUE ';'.                         
000332     03  FILLER              PIC X(1)  VALUE ';'.                         
000333                                                                          
000334 01  U-RUBRIK-RAD6A.                                                      
000335     03  FILLER            PIC X(11) VALUE 'DC NUMBER:'.                  
000336     03  LIST-IDDC-SND     PIC 9(2)  VALUE ZERO.                          
000337     03  FILLER            PIC X(10) VALUE SPACE.                         
000338     03  FILLER            PIC X(11) VALUE 'INVOICE NO.'.                 
000339     03  FILLER            PIC X(1)  VALUE SPACE.                         
000340     03  LIST-IDBYTFAK     PIC Z(3)9 VALUE ZERO.                          
000341     03  FILLER            PIC X(5)  VALUE SPACE.                         
000342     03  FILLER            PIC X(10) VALUE 'DISTR.NO. '.                  
000343     03  LIST-IDDISTR      PIC Z(4)9 VALUE ZERO.                          
000344                                                                          
000345 01  E-U-RUBRIK-RAD6A.                                                    
000346     03  FILLER              PIC X(11) VALUE 'DC NUMBER:'.                
000347     03  E-LIST-IDDC-SND     PIC 9(2)  VALUE ZERO.                        
000348     03  FILLER              PIC X(1)  VALUE ';'.                         
000349     03  FILLER              PIC X(1)  VALUE ';'.                         
000350     03  FILLER              PIC X(11) VALUE 'INVOICE NO.'.               
000351     03  FILLER              PIC X(1)  VALUE SPACE.                       
000352     03  E-LIST-IDBYTFAK     PIC Z(3)9 VALUE ZERO.                        
000353     03  FILLER              PIC X(1)  VALUE ';'.                         
000354     03  FILLER              PIC X(1)  VALUE ';'.                         
000355     03  FILLER              PIC X(10) VALUE 'DISTR.NO. '.                
000356     03  E-LIST-IDDISTR      PIC Z(4)9 VALUE ZERO.                        
000357     03  FILLER              PIC X(1)  VALUE ';'.                         
000358     03  FILLER              PIC X(1)  VALUE ';'.                         
000359     03  FILLER              PIC X(1)  VALUE ';'.                         
000360     03  FILLER              PIC X(1)  VALUE ';'.                         
000361     03  FILLER              PIC X(1)  VALUE ';'.                         
000362                                                                          
000363 01  U-RUBRIK-LINJE.                                                      
000364     03  FILLER            PIC X(92) VALUE ALL '_'.                       
000365                                                                          
000366 01  E-U-RUBRIK-LINJE.                                                    
000367     03  FILLER            PIC X(9) VALUE ALL ';;;;;;;;;'.                
000368                                                                          
000369 01  U-RUBRIK-RAD6C.                                                      
000370     03  FILLER            PIC X(3) VALUE SPACE.                          
000371     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
000372     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
000373     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
000374     03  FILLER            PIC X(8) VALUE 'VALUE **'.                     
000375                                                                          
000376 01  E-U-RUBRIK-RAD6C.                                                    
000377     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
000378     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
000379     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
000380     03  FILLER            PIC X(8) VALUE 'VALUE * '.                     
000381     03  FILLER            PIC X(9) VALUE ';;;;;;;;;'.                    
000382                                                                          
000383 01  SISTA-RADEN1.                                                        
000384     03  FILLER            PIC X(14)      VALUE 'NET WEIGHT '.            
000385     03  LIST-SUVKART-TOT  PIC Z(7)9.9(2) VALUE ZERO.                     
000386     03  FILLER            PIC X(1)       VALUE SPACE.                    
000387     03  LIST-UOM-TWEIGHT  PIC X(3)       VALUE SPACE.                    
000388                                                                          
000389 01  E-SISTA-RADEN1.                                                      
000390     03  FILLER              PIC X(14)    VALUE 'NET WEIGHT '.            
000391     03  E-LIST-SUVKART-TOT  PIC Z(7)9.9(2) VALUE ZERO.                   
000392     03  E-LIST-UOM-TWEIGHT  PIC X(4)       VALUE SPACE.                  
000393     03  FILLER              PIC X(1)       VALUE ';'.                    
000394     03  FILLER              PIC X(1)       VALUE ';'.                    
000395     03  FILLER              PIC X(1)       VALUE ';'.                    
000396     03  FILLER              PIC X(1)       VALUE ';'.                    
000397     03  FILLER              PIC X(1)       VALUE ';'.                    
000398     03  FILLER              PIC X(1)       VALUE ';'.                    
000399     03  FILLER              PIC X(1)       VALUE ';'.                    
000400     03  FILLER              PIC X(1)       VALUE ';'.                    
000401                                                                          
000402 01  SISTA-RADEN2.                                                        
000403     03  FILLER            PIC X(17)     VALUE 'GROSS WEIGHT '.           
000404     03  LIST-GROSS-WEIGHT PIC Z(7)9     VALUE ZERO.                      
000405     03  FILLER            PIC X(1)      VALUE SPACE.                     
000406     03  LIST-UOM-WEIGHT   PIC X(3)      VALUE SPACE.                     
000407                                                                          
000408 01  E-SISTA-RADEN2.                                                      
000409     03  FILLER              PIC X(17)     VALUE 'GROSS WEIGHT '.         
000410     03  E-LIST-GROSS-WEIGHT PIC Z(7)9     VALUE ZERO.                    
000411     03  E-LIST-UOM-WEIGHT   PIC X(4)      VALUE SPACE.                   
000412     03  FILLER              PIC X(1)      VALUE ';'.                     
000413     03  FILLER              PIC X(1)      VALUE ';'.                     
000414     03  FILLER              PIC X(1)      VALUE ';'.                     
000415     03  FILLER              PIC X(1)      VALUE ';'.                     
000416     03  FILLER              PIC X(1)      VALUE ';'.                     
000417     03  FILLER              PIC X(1)      VALUE ';'.                     
000418     03  FILLER              PIC X(1)      VALUE ';'.                     
000419     03  FILLER              PIC X(1)      VALUE ';'.                     
000420                                                                          
000421 01  SISTA-RADEN3.                                                        
000422     03  FILLER             PIC X(17)     VALUE 'TOTAL VOLUME '.          
000423     03  LIST-VLORDBTO-TOT  PIC Z(6).9(2) VALUE ZERO.                     
000424     03  FILLER             PIC X(1)      VALUE SPACE.                    
000425     03  LIST-UOM-VOLUME    PIC X(9)      VALUE SPACE.                    
000426                                                                          
000427 01  E-SISTA-RADEN3.                                                      
000428     03  FILLER               PIC X(17)     VALUE 'TOTAL VOLUME '.        
000429     03  E-LIST-VLORDBTO-TOT  PIC Z(6).9(2) VALUE ZERO.                   
000430     03  E-LIST-UOM-VOLUME    PIC X(10)     VALUE SPACE.                  
000431     03  FILLER               PIC X(1)      VALUE ';'.                    
000432     03  FILLER               PIC X(1)      VALUE ';'.                    
000433     03  FILLER               PIC X(1)      VALUE ';'.                    
000434     03  FILLER               PIC X(1)      VALUE ';'.                    
000435     03  FILLER               PIC X(1)      VALUE ';'.                    
000436     03  FILLER               PIC X(1)      VALUE ';'.                    
000437     03  FILLER               PIC X(1)      VALUE ';'.                    
000438     03  FILLER               PIC X(1)      VALUE ';'.                    
000439                                                                          
000440 01  SISTA-RADEN4.                                                        
000441     03  FILLER            PIC X(17)  VALUE SPACE.                        
000442     03  FILLER            PIC X(17)  VALUE 'TOTAL NO. CASES '.           
000443     03  LIST-IDBYTKOL-TOT PIC Z(5)9 VALUE ZERO.                          
000444     03  FILLER            PIC X(3)  VALUE SPACE.                         
000445     03  FILLER            PIC X(12)     VALUE 'TOTAL VALUE '.            
000446     03  LIST-SUFKTUTL-TOT PIC Z(7).9(2) VALUE ZERO.                      
000447     03  FILLER            PIC X(1)  VALUE SPACE.                         
000448     03  LIST-KDVALISO     PIC X(3).                                      
000449     03  FILLER            PIC X(60) VALUE SPACE.                         
000450                                                                          
000451 01  E-SISTA-RADEN4.                                                      
000452     03  FILLER              PIC X(17)  VALUE 'TOTAL NO. CASES '.         
000453     03  E-LIST-IDBYTKOL-TOT PIC Z(5)9 VALUE ZERO.                        
000454     03  FILLER              PIC X(1)  VALUE ';'.                         
000455     03  FILLER              PIC X(12)     VALUE 'TOTAL VALUE '.          
000456     03  E-LIST-SUFKTUTL-TOT PIC Z(7).9(2) VALUE ZERO.                    
000457     03  FILLER              PIC X(1)  VALUE SPACE.                       
000458     03  E-LIST-KDVALISO     PIC X(3).                                    
000459     03  FILLER              PIC X(1)  VALUE ';'.                         
000460     03  FILLER              PIC X(1)  VALUE ';'.                         
000461     03  FILLER              PIC X(1)  VALUE ';'.                         
000462     03  FILLER              PIC X(1)  VALUE ';'.                         
000463     03  FILLER              PIC X(1)  VALUE ';'.                         
000464     03  FILLER              PIC X(1)  VALUE ';'.                         
000465     03  FILLER              PIC X(1)  VALUE ';'.                         
000466                                                                          
000467 01  SISTA-RADEN5.                                                        
000468     03  FILLER            PIC X(8)  VALUE '** NO CO'.                    
000469     03  FILLER            PIC X(19) VALUE 'MMERCIAL VALUE STAT'.         
000470     03  FILLER            PIC X(19) VALUE 'ED FOR CUSTOMS PURP'.         
000471     03  FILLER            PIC X(12) VALUE 'OSES ONLY **'.                
000472     03  FILLER            PIC X(70) VALUE SPACE.                         
000473                                                                          
000474 01  E-SISTA-RADEN5.                                                      
000475     03  FILLER            PIC X(8)  VALUE '** NO CO'.                    
000476     03  FILLER            PIC X(19) VALUE 'MMERCIAL VALUE STAT'.         
000477     03  FILLER            PIC X(19) VALUE 'ED FOR CUSTOMS PURP'.         
000478     03  FILLER            PIC X(12) VALUE 'OSES ONLY * '.                
000479     03  FILLER            PIC X(8)  VALUE ';;;;;;;;'.                    
000480                                                                          
000481 01  SISTA-RADEN6.                                                        
000482     03  FILLER            PIC X(8)  VALUE 'SHIPPERS'.                    
000483     03  FILLER            PIC X(15) VALUE ' SIGNATURE AND '.             
000484     03  FILLER            PIC X(19) VALUE 'TITLE ____________ '.         
000485     03  FILLER            PIC X(70) VALUE ALL '_'.                       
000486                                                                          
000487 01  E-SISTA-RADEN6.                                                      
000488     03  FILLER            PIC X(8)  VALUE 'SHIPPERS'.                    
000489     03  FILLER            PIC X(15) VALUE ' SIGNATURE AND '.             
000490     03  FILLER            PIC X(19) VALUE 'TITLE ____________ '.         
000491     03  FILLER            PIC X(20) VALUE '____________________'.        
000492     03  FILLER            PIC X(1)  VALUE ';'.                           
000493     03  FILLER            PIC X(1)  VALUE ';'.                           
000494     03  FILLER            PIC X(1)  VALUE ';'.                           
000495     03  FILLER            PIC X(1)  VALUE ';'.                           
000496     03  FILLER            PIC X(1)  VALUE ';'.                           
000497     03  FILLER            PIC X(1)  VALUE ';'.                           
000498     03  FILLER            PIC X(1)  VALUE ';'.                           
000499     03  FILLER            PIC X(1)  VALUE ';'.                           
000500                                                                          
000501                                                                          
000502 01  U-RUBRIK-RAD7.                                                       
000503     03  FILLER            PIC X(9) VALUE '  PART NO'.                    
000504     03  FILLER            PIC X(1) VALUE SPACE.                          
000505     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
000506     03  FILLER            PIC X(10) VALUE SPACE.                         
000507     03  FILLER            PIC X(1) VALUE SPACE.                          
000508     03  FILLER            PIC X(5) VALUE 'Q.DEL' .                       
000509     03  FILLER            PIC X(4) VALUE SPACE.                          
000510     03  FILLER            PIC X(7) VALUE 'U-PRICE' .                     
000511     03  FILLER            PIC X(4) VALUE SPACE.                          
000512     03  FILLER            PIC X(7) VALUE 'T-PRICE' .                     
000513     03  FILLER            PIC X(1) VALUE SPACE.                          
000514     03  FILLER            PIC X(4) VALUE 'CASE'.                         
000515     03  FILLER            PIC X(1) VALUE SPACE.                          
000516     03  FILLER            PIC X(3) VALUE 'ORG'.                          
000517     03  FILLER            PIC X(4) VALUE SPACE.                          
000518     03  FILLER            PIC X(8) VALUE 'U-WEIGHT'.                     
000519     03  FILLER            PIC X(3) VALUE SPACE.                          
000520     03  FILLER            PIC X(8) VALUE 'T-WEIGHT'.                     
000521                                                                          
000522 01  E-U-RUBRIK-RAD7.                                                     
000523     03  FILLER            PIC X(9) VALUE '  PART NO'.                    
000524     03  FILLER            PIC X(1)  VALUE ';'.                           
000525     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
000526     03  FILLER            PIC X(1)  VALUE ';'.                           
000527     03  FILLER            PIC X(5) VALUE 'Q.DEL' .                       
000528     03  FILLER            PIC X(1)  VALUE ';'.                           
000529     03  FILLER            PIC X(7) VALUE 'U-PRICE' .                     
000530     03  FILLER            PIC X(1)  VALUE ';'.                           
000531     03  FILLER            PIC X(7) VALUE 'T-PRICE' .                     
000532     03  FILLER            PIC X(1)  VALUE ';'.                           
000533     03  FILLER            PIC X(4) VALUE 'CASE'.                         
000534     03  FILLER            PIC X(1)  VALUE ';'.                           
000535     03  FILLER            PIC X(3) VALUE 'ORG'.                          
000536     03  FILLER            PIC X(1)  VALUE ';'.                           
000537     03  FILLER            PIC X(8) VALUE 'U-WEIGHT'.                     
000538     03  FILLER            PIC X(1)  VALUE ';'.                           
000539     03  FILLER            PIC X(8) VALUE 'T-WEIGHT'.                     
000540     03  FILLER            PIC X(1)  VALUE ';'.                           
000541                                                                          
000542 01  U-RAD.                                                               
000543     03  LIST-IDARTNR      PIC Z(9).                                      
000544     03  FILLER            PIC X(1) VALUE SPACE.                          
000545     03  LIST-BEART        PIC X(20).                                     
000546     03  FILLER            PIC X(2) VALUE SPACE.                          
000547     03  LIST-KVLEVART     PIC Z(4).                                      
000548     03  FILLER            PIC X(1) VALUE SPACE.                          
000549     03  FILLER            PIC X(1) VALUE ' '.                            
000550     03  LIST-PRFKTUTL     PIC Z(6).9(2) VALUE ZERO.                      
000551     03  FILLER            PIC X(1) VALUE SPACE.                          
000552     03  FILLER            PIC X(1) VALUE ' '.                            
000553     03  LIST-PRFKTUTL-RAD PIC Z(6).9(2) VALUE ZERO.                      
000554     03  FILLER            PIC X(2) VALUE SPACE.                          
000555     03  LIST-IDKOLLI      PIC Z(3)9 VALUE ZERO.                          
000556     03  FILLER            PIC X(1) VALUE SPACE.                          
000557     03  LIST-KDARTURS     PIC X(3) VALUE SPACE.                          
000558     03  FILLER            PIC X(1) VALUE SPACE.                          
000559     03  LIST-VKART-UNIT   PIC Z(6)9.9(2) VALUE ZERO.                     
000560     03  FILLER            PIC X(1) VALUE SPACE.                          
000561     03  LIST-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                     
000562                                                                          
000563 01  E-U-RAD.                                                             
000564     03  E-LIST-IDARTNR      PIC Z(9).                                    
000565     03  FILLER              PIC X(1)  VALUE ';'.                         
000566     03  E-LIST-BEART        PIC X(20).                                   
000567     03  FILLER              PIC X(1)  VALUE ';'.                         
000568     03  E-LIST-KVLEVART     PIC Z(4).                                    
000569     03  FILLER              PIC X(1)  VALUE ';'.                         
000570     03  E-LIST-PRFKTUTL     PIC Z(6).9(2) VALUE ZERO.                    
000571     03  FILLER              PIC X(1)  VALUE ';'.                         
000572     03  E-LIST-PRFKTUTL-RAD PIC Z(6).9(2) VALUE ZERO.                    
000573     03  FILLER              PIC X(1)  VALUE ';'.                         
000574     03  E-LIST-IDKOLLI      PIC Z(3)9 VALUE ZERO.                        
000575     03  FILLER              PIC X(1)  VALUE ';'.                         
000576     03  E-LIST-KDARTURS     PIC X(3) VALUE SPACE.                        
000577     03  FILLER              PIC X(1)  VALUE ';'.                         
000578     03  E-LIST-VKART-UNIT   PIC Z(6)9.9(2) VALUE ZERO.                   
000579     03  FILLER              PIC X(1)  VALUE ';'.                         
000580     03  E-LIST-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                   
000581     03  FILLER              PIC X(1)  VALUE ';'.                         
000582     EJECT                                                                
000583 01  OMV-AREA-START              PIC X(24)   VALUE                        
000584                                             'OMV-AREA-START'.            
000585     SKIP2                                                                
000586                                                                          
000587*01  -COPY WWOMVAND                                                       
000588     EJECT                                                                
000589 01  A15-AREA-START              PIC X(24)   VALUE                        
000590                                             'A15-AREA-START'.            
000591     SKIP2                                                                
000592                                                                          
000593*01  AREA -COPY W510A15     -PRE A15-                                     
000594     EJECT                                                                
000595 01  EKH-AREA-START              PIC X(24)   VALUE                        
000596                                             'EKH-AREA-START'.            
000597     SKIP2                                                                
000598                                                                          
000599*01  -COPY W510EKHA                                                       
000600     EJECT                                                                
000601 01  BYTES-AREA-START            PIC X(24)   VALUE                        
000602                                             'BYTES-AREA-START'.          
000603     SKIP2                                                                
000604                                                                          
000605*01  AREA -COPY W37116     -PRE BYTES-                                    
000606     EJECT                                                                
000607 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000608     SKIP3                                                                
000609                                                                          
000610 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
000611 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
000612 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
000613 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
000614                                                                          
000615 01  NYCKLAR-TILL-DLI.                                                    
000616     03  W-IDGMT-MIN-X.                                                   
000617         05  W-IDDISTR-1     PIC S9(5)   VALUE ZERO COMP-3.               
000618         05  W-IDKUNDNR-1    PIC S9(7)   VALUE ZERO COMP-3.               
000619     03  W-IDGMT-MAX-X.                                                   
000620         05  W-IDDISTR-2     PIC S9(5)   VALUE ZERO COMP-3.               
000621         05  W-IDKUNDNR-2    PIC S9(7)   VALUE +9999999 COMP-3.           
000622     03  W-KDSEGKEY-X.                                                    
000623         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
000624     03  W-IDDC-X.                                                        
000625         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000626     03  W-WDGXKEY-X.                                                     
000627         05  FILLER              PIC X(4)    VALUE '3165'.                
000628         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000629     03  W-IDBYTFAK-X.                                                    
000630         05  W-IDBYTFAK          PIC 9(4)   VALUE ZERO.                   
000631     03  W-WDGX3168-X.                                                    
000632         05  W-IDBYTKOL          PIC 9(3)   VALUE ZERO.                   
000633         05  W-IDARTNR-OBJ       PIC S9(9)  VALUE ZERO COMP-3.            
000634     03  W-IDARTNR-X.                                                     
000635         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000636     03  W-IDSKYLT-X.                                                     
000637         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000638     SKIP2                                                                
000639*    --- STATUS-KOD FRÅN IMS                                              
000640 01  STATUS-WS                   PIC XX.                                  
000641     88  SEGMENT-FINNS                       VALUE '  '.                  
000642     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000643     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000644     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000645     88  IMS-EJ-OK                           VALUE 'XD'.                  
000646     SKIP2                                                                
000647 01  GODK-STATUSKODER.                                                    
000648     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000649     SKIP3                                                                
000650 01  SSA1                        PIC X(80).                               
000651 01  SSA2                        PIC X(80).                               
000652     EJECT                                                                
000653*    --- IMS FUNKTIONSKODER                                               
000654*01  -COPY W0003                                                          
000655     EJECT                                                                
000656*    ---  DLI INPUT-OUTPUT AREA                                           
000657                                                                          
000658 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTA01'.                    
000659 01  DLI-IO-WLGMTA01.                                                     
000660*    03  -COPY WDB201  -PRE GMTA-                                         
000661 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
000662 01  DLI-IO-WLARTC01.                                                     
000663*    03  -COPY WDK601  -PRE ARTC-                                         
000664     EJECT                                                                
000665 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
000666 01  DLI-IO-WLARTC11.                                                     
000667*    03  -COPY WDK611  -PRE ARTC11-                                       
000668 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
000669 01  DLI-IO-WLARTS01.                                                     
000670*    03  -COPY WDK701  -PRE ARTS-                                         
000671     EJECT                                                                
000672 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
000673 01  DLI-IO-WLARTS11.                                                     
000674*    03  -COPY WDK711  -PRE ARTS11-                                       
000675 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL316501'.                    
000676 01  DLI-IO-WL316501.                                                     
000677*    03  -COPY WDGX3165 -PRE H3165-                                       
000678     EJECT                                                                
000679 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL316511'.                    
000680 01  DLI-IO-WL316511.                                                     
000681*    03  -COPY WDGX3166 -PRE H3166-                                       
000682     EJECT                                                                
000683 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL316521'.                    
000684 01  DLI-IO-WL316521.                                                     
000685*    03  -COPY WDGX3168 -PRE H3168-                                       
000686 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA01'.                    
000687 01  DLI-IO-WLBENA01.                                                     
000688*    03  -COPY WDD3B1  -PRE BENA-                                         
000689     EJECT                                                                
000690 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA11'.                    
000691 01  DLI-IO-WLBENA11.                                                     
000692*    03  -COPY WDD311  -PRE BENA-                                         
000693     EJECT                                                                
000694 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR801'.                      
000695 01  DLI-IO-WDR801.                                                       
000696*    03  -COPY WDR801                                                     
000697     EJECT                                                                
000698 LINKAGE SECTION.                                                         
000699                                                                          
000700*01  -COPY W0009   -PRE MSG-                                              
000701     EJECT                                                                
000702*01  -COPY W0008  -PRE GMTA-                                              
000703     05  FILLER                  PIC X.                                   
000704     EJECT                                                                
000705*01  -COPY W0008  -PRE ARTC-                                              
000706     05  FILLER                  PIC X.                                   
000707     EJECT                                                                
000708*01  -COPY W0008  -PRE ARTS-                                              
000709     05  FILLER                  PIC X.                                   
000710     EJECT                                                                
000711*01  -COPY W0008  -PRE 3165-                                              
000712     05  FILLER                  PIC X.                                   
000713     EJECT                                                                
000714*01  -COPY W0008  -PRE BENA-                                              
000715     05  FILLER                  PIC X.                                   
000716     EJECT                                                                
000717*01  -COPY W0008  -PRE  WDG2-                                             
000718     05  FILLER                 PIC X.                                    
000719                                                                          
000720*01  -COPY W0008 -PRE  PRIS-ARTC-.                                        
000721     05  FILLER        PIC X.                                             
000722     EJECT                                                                
000723                                                                          
000724*01  -COPY W0008 -PRE  PRIS-WDK7-.                                        
000725     05  FILLER        PIC X.                                             
000726     EJECT                                                                
000727                                                                          
000728*01  -COPY W0008 -PRE  PRIS-GMTA-.                                        
000729     05  FILLER        PIC X.                                             
000730     EJECT                                                                
000731                                                                          
000732*01  -COPY W0008 -PRE  PRIS-BETA-.                                        
000733     05  FILLER        PIC X.                                             
000734                                                                          
000735*01  -COPY W0008 -PRE  PRIS-GPRIA-.                                       
000736     05  FILLER        PIC X.                                             
000737     EJECT                                                                
000738*01  -COPY W0008 -PRE  PRIS-GPRIB-.                                       
000739     05  FILLER        PIC X.                                             
000740     EJECT                                                                
000741*01  -COPY W0008 -PRE  WDR8-.                                             
000742     05  FILLER        PIC X.                                             
000743     EJECT                                                                
000744 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000745 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000746 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000747 01  PRIS-COST-9305-PCB          PIC X.                                   
000748 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000749 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000750     EJECT                                                                
000751 PROCEDURE DIVISION  USING MSG-PCB                                        
000752     GMTA-PCB ARTC-PCB ARTS-PCB 3165-PCB BENA-PCB                         
000753     WDG2-PCB                                                             
000754     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
000755     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
000756     PRIS-COST-WDK6-PCB                                                   
000757     PRIS-COST-WDK7-PCB                                                   
000758     PRIS-COST-WDF1-PCB                                                   
000759     PRIS-COST-9305-PCB                                                   
000760     PRIS-COST-WDK72-PCB                                                  
000761     PRIS-COST-WDB6-PCB                                                   
000762     WDR8-PCB.                                                            
000763 MAIN SECTION.                                                            
000764     ENTRY 'DLITCBL' USING MSG-PCB                                        
000765     GMTA-PCB ARTC-PCB ARTS-PCB 3165-PCB BENA-PCB                         
000766     WDG2-PCB                                                             
000767     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
000768     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
000769     PRIS-COST-WDK6-PCB                                                   
000770     PRIS-COST-WDK7-PCB                                                   
000771     PRIS-COST-WDF1-PCB                                                   
000772     PRIS-COST-9305-PCB                                                   
000773     PRIS-COST-WDK72-PCB                                                  
000774     PRIS-COST-WDB6-PCB                                                   
000775     WDR8-PCB.                                                            
000776                                                                          
000777     PERFORM A-INIT                                                       
000778                                                                          
000779     IF FAKTURA-HITTAD                                                    
000780       PERFORM B-INIT                                                     
000781       MOVE JA TO SKRIV-RUBRIK-SW                                         
000782*****LÄS FRAM TILL FÖRSTA RADEN I FAKTURAN *****!!                        
000783       PERFORM IMS-GET-3165-H3168-FIRST                                   
000784       PERFORM C-FAKTURAHUVUD                                             
000785       IF SEGMENT-SAKNAS                                                  
000786          MOVE U-RUBRIK-LINJE TO WS-RAD                                   
000787          PERFORM S01-SKRIV-RAD                                           
000788          MOVE SPACE TO WS-RAD                                            
000789          PERFORM F-SKAPA-SLUTSIDA                                        
000790*NYCKEL FINNS  UPPLAGD W-IDBYTFAK                                         
000791          PERFORM IMS-GET-UNIK-H3166                                      
000792          PERFORM IMS-DLET-3165                                           
000793       ELSE                                                               
000794          PERFORM UNTIL SEGMENT-SAKNAS OR FAKTURAN-SLUT                   
000795                                                                          
000796             MOVE +1 TO INDX                                              
000797             PERFORM UNTIL INDX > 30 OR SEGMENT-SAKNAS                    
000798                PERFORM D-BEHANDLA-RAD                                    
000799                PERFORM IMS-GET-3165-H3168                                
000800                ADD +1 TO INDX                                            
000801             END-PERFORM                                                  
000802                                                                          
000803             MOVE U-RUBRIK-LINJE TO WS-RAD                                
000804             PERFORM S01-SKRIV-RAD                                        
000805             MOVE SPACE TO WS-RAD                                         
000806                                                                          
000807             IF SEGMENT-FINNS                                             
000808               PERFORM C-FAKTURAHUVUD                                     
000809             ELSE                                                         
000810               PERFORM F-SKAPA-SLUTSIDA                                   
000811*NYCKEL FINNS  UPPLAGD W-IDBYTFAK                                         
000812               PERFORM IMS-GET-UNIK-H3166                                 
000813               PERFORM IMS-DLET-3165                                      
000814               MOVE JA TO FAKTURAN-SLUT-SW                                
000815             END-IF                                                       
000816                                                                          
000817          END-PERFORM                                                     
000818                                                                          
000819       END-IF                                                             
000820                                                                          
000821     ELSE                                                                 
000822       DISPLAY ' HITTADE INGEN FAKTURA'                                   
000823     END-IF                                                               
000824                                                                          
000825     PERFORM Z-FINIT                                                      
000826                                                                          
000827     MOVE ZERO TO RETURN-CODE                                             
000828     GOBACK                                                               
000829     .                                                                    
000830     EJECT                                                                
000831 A-INIT SECTION.                                                          
000832                                                                          
000833     OPEN OUTPUT W3717H                                                   
000834     OPEN OUTPUT W37178                                                   
000835     OPEN OUTPUT W3717C                                                   
000836                                                                          
000837     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
000838     MOVE D-MAANAD                        TO W-DATE-AAMM(3:2)             
000839     MOVE D-AAR                           TO LIST-AAR                     
000840                                             W-DATE-AAMM(1:2)             
000841     MOVE D-MAANAD                        TO LIST-MAN                     
000842     MOVE D-DAG                           TO LIST-DAG                     
000843                                                                          
000844     MOVE D-AAR                           TO E-LIST-AAR                   
000845     MOVE D-MAANAD                        TO E-LIST-MAN                   
000846     MOVE D-DAG                           TO E-LIST-DAG                   
000847     MOVE W-DATE-AAMM                     TO CURR-TIAAMM                  
000848     MOVE WS-KDVALISO-HUV                 TO CURR-KDVALISO-HUV            
000849     MOVE 'M'                             TO CURR-KDVALTYP                
000850                                                                          
000851     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000852*****LÄS FRAM TILL RÄTT HÄNDELSE POST *****!!                             
000853*****LÄS FRAM TILL AKTUELL FAKTURA *****!!                                
000854     PERFORM IMS-GET-3165-H3165                                           
000855     IF SEGMENT-FINNS                                                     
000856       PERFORM IMS-GET-3165-H3166                                         
000857       PERFORM UNTIL FAKTURA-HITTAD OR SEGMENT-SLUT                       
000858                                    OR SEGMENT-SAKNAS                     
000859         IF H3166-3166-KDBYTFAK = '2'                                     
000860           MOVE JA TO FAKTURA-HITTAD-SW                                   
000861         ELSE                                                             
000862           PERFORM IMS-GET-3165-H3166                                     
000863         END-IF                                                           
000864       END-PERFORM                                                        
000865     ELSE                                                                 
000866       CALL ABEND                                                         
000867     END-IF                                                               
000868     MOVE H3166-3166-IDDC TO WS-IDDC                                      
000869     IF NDC-NA                                                            
000870        COMPUTE WS-VLORDBTO-UTAN-DEC ROUNDED =                            
000871                H3166-3166-VLORDBTO-FAKT * CONV-M3-TO-FT3                 
000872        MOVE WS-VLORDBTO-UTAN-DEC  TO LIST-VLORDBTO-TOT                   
000873                                      E-LIST-VLORDBTO-TOT                 
000874        MOVE 'C.U. FEET'           TO LIST-UOM-VOLUME                     
000875        MOVE ' C.U. FEET'          TO E-LIST-UOM-VOLUME                   
000876                                                                          
000877        COMPUTE WS-VKORDBTO-UTAN-DEC ROUNDED =                            
000878                H3166-3166-VKORDBTO-FAKT * CONV-KG-TO-LB                  
000879        MOVE WS-VKORDBTO-UTAN-DEC  TO LIST-GROSS-WEIGHT                   
000880                                      E-LIST-GROSS-WEIGHT                 
000881        MOVE 'LBS'                 TO LIST-UOM-WEIGHT                     
000882        MOVE ' LBS'                TO E-LIST-UOM-WEIGHT                   
000883     ELSE                                                                 
000884       MOVE H3166-3166-VLORDBTO-FAKT                                      
000885                                   TO LIST-VLORDBTO-TOT                   
000886                                      E-LIST-VLORDBTO-TOT                 
000887       MOVE 'M³       '            TO LIST-UOM-VOLUME                     
000888       MOVE ' M³       '           TO E-LIST-UOM-VOLUME                   
000889                                                                          
000890       MOVE H3166-3166-VKORDBTO-FAKT                                      
000891                                   TO LIST-GROSS-WEIGHT                   
000892                                      E-LIST-GROSS-WEIGHT                 
000893       MOVE 'KG '                  TO LIST-UOM-WEIGHT                     
000894       MOVE ' KG '                 TO E-LIST-UOM-WEIGHT                   
000895     END-IF                                                               
000896     .                                                                    
000897     EJECT                                                                
000898 B-INIT SECTION.                                                          
000899                                                                          
000900     MOVE NEJ TO FL-WEBDC-SW                                              
000901                                                                          
000902     MOVE H3166-3166-IDDC TO LIST-IDDC-SND                                
000903                             WS-IDDC                                      
000904                            E-LIST-IDDC-SND                               
000905                                                                          
000906     IF NDC-US-RU                                                         
000907        CONTINUE                                                          
000908     ELSE                                                                 
000909        IF NDC-US-LA OR NDC-US-SE OR NDC-US-CH OR NDC-US-JA OR            
000910           NDC-US-DA OR NDC-CA OR NDC-KR OR NDC-MY OR NDC-TH OR           
000911           NDC-TW OR NDC-CN-71                                            
000912           MOVE JA TO FL-WEBDC-SW                                         
000913        ELSE                                                              
000914           DISPLAY 'EJ GODKÄNT LAGERID' H3166-3166-IDDC                   
000915*          CALL ABEND                                                     
000916        END-IF                                                            
000917     END-IF                                                               
000918     DISPLAY 'IDDC ' H3166-3166-IDDC                                      
000919     DISPLAY 'IDBYTFAK ' H3166-3166-IDBYTFAK                              
000920     DISPLAY 'DAFAKT ' H3166-3166-DAFAKT                                  
000921                                                                          
000922     MOVE H3166-3166-IDBYTFAK TO W-IDBYTFAK                               
000923                                 LIST-IDBYTFAK                            
000924                               E-LIST-IDBYTFAK                            
000925     MOVE H3166-3166-IDDC    TO WS-IDDC                                   
000926                                                                          
000927     MOVE H3166-3166-IDDISTR TO SPAR-IDDISTR                              
000928                                LIST-IDDISTR                              
000929                                E-LIST-IDDISTR                            
000930                                                                          
000931                                                                          
000932     IF H3166-3166-IDDISTR = 9927 OR 9993                                 
000933        MOVE JA                  TO SKRIV-BYTES-RAPPORT                   
000934        MOVE 'NDC'               TO BYTES-IDPTYP                          
000935        MOVE H3166-3166-IDBYTFAK TO WS-IDBYTFAK                           
000936                                   BYTES-IDFAKT                           
000937        MOVE ZERO                TO BYTES-IDDISTR-FEL                     
000938                                                                          
000939        MOVE H3166-3166-DAFAKT  TO BYTES-TIREGDAT                         
000940        ADD +1000000            TO BYTES-TIREGDAT                         
000941*FUNKTIONEN HÄMTAR          DATUM 19960909                                
000942*BYTES DAT FÅR DÅ           DATUM  9960909                                
000943*GENOM ADD AV 1000000 BLIR  DATUM  0960909                                
000944*ÅR 2000 BLIR DATUM                1000909 OCH ALLTSÅ STÖRRE              
000945*ÄN 1900-TALET.                                                           
000946        MOVE ZERO               TO BYTES-IDKUNDNR                         
000947                                   BYTES-IDORDNR7                         
000948                                   BYTES-IDTABNR                          
000949                                   BYTES-IDBYTRAD                         
000950        MOVE SPACE              TO BYTES-BERADREF                         
000951        MOVE NEJ                TO BYTES-FLBYTGAR                         
000953        IF H3166-3166-IDDISTR = 9927                                      
000954          MOVE WC-SDC-NL-ET       TO BYTES-IDDC                           
000955        ELSE                                                              
000956          MOVE WC-NDC-TH-93       TO BYTES-IDDC                           
000957        END-IF                                                            
000958     END-IF                                                               
000959                                                                          
000960     PERFORM BA-HAEMTA-KUND-INFO                                          
000961     PERFORM BB-RAEKNA-FAKTURA-RADER                                      
000962     .                                                                    
000963     EJECT                                                                
000964                                                                          
000965 BA-HAEMTA-KUND-INFO SECTION.                                             
000966     SKIP2                                                                
000967     MOVE SPAR-IDDISTR TO  W-IDDISTR-1                                    
000968                           W-IDDISTR-2                                    
000969                                                                          
000970     MOVE SPACE        TO  LIST-BEGMT-RAD1                                
000971                           LIST-BEGMT-RAD2                                
000972                           LIST-ADGMT-GATA                                
000973                           LIST-ADGMT-PADR                                
000974                           LIST-ADGMT-LAND                                
000975                           LIST-BEAVS-RAD1                                
000976                           LIST-BEAVS-RAD2                                
000977                           LIST-ADAVS-GATA                                
000978                           LIST-ADAVS-PADR                                
000979                           LIST-ADAVS-LAND                                
000980                                                                          
000981     MOVE SPACE        TO  E-LIST-BEGMT-RAD1                              
000982                           E-LIST-BEGMT-RAD2                              
000983                           E-LIST-ADGMT-GATA                              
000984                           E-LIST-ADGMT-PADR                              
000985                           E-LIST-ADGMT-LAND                              
000986                           E-LIST-BEAVS-RAD1                              
000987                           E-LIST-BEAVS-RAD2                              
000988                           E-LIST-ADAVS-GATA                              
000989                           E-LIST-ADAVS-PADR                              
000990                           E-LIST-ADAVS-LAND                              
000991                                                                          
000992     PERFORM IMS-GET-GMTA                                                 
000993     IF SEGMENT-FINNS                                                     
000994       MOVE GMTA-GMT-BEGMT-RAD1 TO   LIST-BEGMT-RAD1                      
000995       MOVE GMTA-GMT-BEGMT-RAD2 TO   LIST-BEGMT-RAD2                      
000996       MOVE GMTA-GMT-ADGMT-GATA TO   LIST-ADGMT-GATA                      
000997       MOVE GMTA-GMT-ADGMT-PADR TO   LIST-ADGMT-PADR                      
000998       MOVE GMTA-GMT-ADGMT-LAND TO   LIST-ADGMT-LAND                      
000999                                                                          
001000       MOVE GMTA-GMT-BEGMT-RAD1 TO   E-LIST-BEGMT-RAD1                    
001001       MOVE GMTA-GMT-BEGMT-RAD2 TO   E-LIST-BEGMT-RAD2                    
001002       MOVE GMTA-GMT-ADGMT-GATA TO   E-LIST-ADGMT-GATA                    
001003       MOVE GMTA-GMT-ADGMT-PADR TO   E-LIST-ADGMT-PADR                    
001004       MOVE GMTA-GMT-ADGMT-LAND TO   E-LIST-ADGMT-LAND                    
001005     ELSE                                                                 
001006       MOVE 'MISSING '          TO   LIST-BEGMT-RAD1                      
001007       MOVE 'MISSING '          TO   LIST-BEGMT-RAD2                      
001008       MOVE 'MISSING '          TO   LIST-ADGMT-GATA                      
001009       MOVE 'MISSING '          TO   LIST-ADGMT-PADR                      
001010       MOVE 'MISSING '          TO   LIST-ADGMT-LAND                      
001011                                                                          
001012       MOVE 'MISSING '          TO   E-LIST-BEGMT-RAD1                    
001013       MOVE 'MISSING '          TO   E-LIST-BEGMT-RAD2                    
001014       MOVE 'MISSING '          TO   E-LIST-ADGMT-GATA                    
001015       MOVE 'MISSING '          TO   E-LIST-ADGMT-PADR                    
001016       MOVE 'MISSING '          TO   E-LIST-ADGMT-LAND                    
001017                                                                          
001018     END-IF                                                               
001019*- - FÖR BYTES RAPPORTERNA I WDM6 LÄGGS DISTRIKT 7512 ELLER 7625          
001020*- - FÖR USA RESP CANADA                                                  
001021     MOVE +7512 TO BYTES-IDDISTR                                          
001022     IF NDC-US-RU                                                         
001023*- - - - - - RUTHERFORD                                                   
001024       MOVE +8141 TO W-IDDISTR-1                                          
001025                     W-IDDISTR-2                                          
001026     ELSE                                                                 
001027       IF NDC-US-LA                                                       
001028*- - - - - LOS ANGELES                                                    
001029         MOVE +8143 TO W-IDDISTR-1                                        
001030                       W-IDDISTR-2                                        
001031       ELSE                                                               
001032         IF NDC-US-SE                                                     
001033*- - - - - SEATTLE                                                        
001034           MOVE +8144 TO W-IDDISTR-1                                      
001035                         W-IDDISTR-2                                      
001036         ELSE                                                             
001037           IF NDC-US-CH                                                   
001038*- - - - - CHICAGO                                                        
001039             MOVE +8145 TO W-IDDISTR-1                                    
001040                           W-IDDISTR-2                                    
001041           ELSE                                                           
001042             IF NDC-US-JA                                                 
001043*- - - - - JACKSONVILLE                                                   
001044               MOVE +8146 TO W-IDDISTR-1                                  
001045                             W-IDDISTR-2                                  
001046             ELSE                                                         
001047              IF NDC-US-DA                                                
001048*- - - - - DALLAS                                                         
001049               MOVE +8147 TO W-IDDISTR-1                                  
001050                             W-IDDISTR-2                                  
001051              ELSE                                                        
001052               IF NDC-CA                                                  
001053*- - - - - TORONTO CANADA                                                 
001054                 MOVE +8151 TO W-IDDISTR-1                                
001055                               W-IDDISTR-2                                
001056                 MOVE +7625 TO BYTES-IDDISTR                              
001057               ELSE                                                       
001058                 IF NDC-KR                                                
001059*- - - - - KOREA                                                          
001060                   MOVE +8165 TO W-IDDISTR-1                              
001061                                 W-IDDISTR-2                              
001062                   MOVE +6121 TO BYTES-IDDISTR                            
001063                 ELSE                                                     
001064                   IF NDC-MY                                              
001065*- - - - - MALAYSIA                                                       
001066                     MOVE +8166 TO W-IDDISTR-1                            
001067                                   W-IDDISTR-2                            
001068                     MOVE +5619 TO BYTES-IDDISTR                          
001069                   ELSE                                                   
001070                     IF NDC-TH                                            
001071*- - - - - THAILAND                                                       
001072                       MOVE +8163 TO W-IDDISTR-1                          
001073                                     W-IDDISTR-2                          
001074                       MOVE +6251 TO BYTES-IDDISTR                        
001075                     ELSE                                                 
001076                       IF NDC-TW                                          
001077*- - - - - TAIWAN                                                         
001078                         MOVE +8164 TO W-IDDISTR-1                        
001079                                       W-IDDISTR-2                        
001080                         MOVE +6200 TO BYTES-IDDISTR                      
001081                       ELSE                                               
001082                         IF NDC-CN-71                                     
001083*- - - - - CHINA                                                          
001084                           MOVE +8171 TO W-IDDISTR-1                      
001085                                         W-IDDISTR-2                      
001086                           MOVE +6270 TO BYTES-IDDISTR                    
001087                         END-IF                                           
001088                       END-IF                                             
001089                     END-IF                                               
001090                   END-IF                                                 
001091                 END-IF                                                   
001092               END-IF                                                     
001093              END-IF                                                      
001094             END-IF                                                       
001095           END-IF                                                         
001096         END-IF                                                           
001097       END-IF                                                             
001098     END-IF                                                               
001099     MOVE  +0                  TO   WS-SIDA                               
001100     PERFORM IMS-GET-GMTA                                                 
001101     IF SEGMENT-FINNS                                                     
001102        MOVE GMTA-GMT-BEGMT-RAD1 TO   LIST-BEAVS-RAD1                     
001103        MOVE GMTA-GMT-BEGMT-RAD2 TO   LIST-BEAVS-RAD2                     
001104        MOVE GMTA-GMT-ADGMT-GATA TO   LIST-ADAVS-GATA                     
001105        MOVE GMTA-GMT-ADGMT-PADR TO   LIST-ADAVS-PADR                     
001106        MOVE GMTA-GMT-ADGMT-LAND TO   LIST-ADAVS-LAND                     
001107                                                                          
001108        MOVE GMTA-GMT-BEGMT-RAD1 TO   E-LIST-BEAVS-RAD1                   
001109        MOVE GMTA-GMT-BEGMT-RAD2 TO   E-LIST-BEAVS-RAD2                   
001110        MOVE GMTA-GMT-ADGMT-GATA TO   E-LIST-ADAVS-GATA                   
001111        MOVE GMTA-GMT-ADGMT-PADR TO   E-LIST-ADAVS-PADR                   
001112        MOVE GMTA-GMT-ADGMT-LAND TO   E-LIST-ADAVS-LAND                   
001113                                                                          
001114     ELSE                                                                 
001115        MOVE 'MISSING  '         TO   LIST-BEAVS-RAD1                     
001116        MOVE 'MISSING  '         TO   LIST-BEAVS-RAD2                     
001117        MOVE 'MISSING  '         TO   LIST-ADAVS-GATA                     
001118        MOVE 'MISSING  '         TO   LIST-ADAVS-PADR                     
001119        MOVE GMTA-GMT-ADGMT-LAND TO   LIST-ADAVS-LAND                     
001120                                                                          
001121        MOVE 'MISSING  '         TO   E-LIST-BEAVS-RAD1                   
001122        MOVE 'MISSING  '         TO   E-LIST-BEAVS-RAD2                   
001123        MOVE 'MISSING  '         TO   E-LIST-ADAVS-GATA                   
001124        MOVE 'MISSING  '         TO   E-LIST-ADAVS-PADR                   
001125        MOVE GMTA-GMT-ADGMT-LAND TO   E-LIST-ADAVS-LAND                   
001126                                                                          
001127     END-IF                                                               
001128     .                                                                    
001129     EJECT                                                                
001130 BB-RAEKNA-FAKTURA-RADER SECTION.                                         
001131     SKIP2                                                                
001132     PERFORM IMS-GET-3165-H3168                                           
001133     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
001134       ADD +1 TO WS-ANTAL-RADER                                           
001135       PERFORM IMS-GET-3165-H3168                                         
001136       IF WS-ANTAL-RADER > +1000                                          
001137         DISPLAY WS-ANTAL-RADER ' FÖR MÅNGA F-RADER '                     
001138         CALL ABEND                                                       
001139       END-IF                                                             
001140     END-PERFORM                                                          
001141     DIVIDE WS-ANTAL-RADER BY 30 GIVING WS-DUMMY                          
001142                                 REMAINDER WS-REST                        
001143     IF WS-REST = ZERO                                                    
001144       COMPUTE WS-SIDA-MAX ROUNDED = (WS-ANTAL-RADER / 30)                
001145     ELSE                                                                 
001146       COMPUTE WS-SIDA-MAX ROUNDED = (WS-ANTAL-RADER / 30) + 0.5          
001147     END-IF                                                               
001148     MOVE WS-SIDA-MAX TO LIST-SIDA-MAX                                    
001149*                        E-LIST-SIDA-MAX                                  
001150     .                                                                    
001151     EJECT                                                                
001152                                                                          
001153 C-FAKTURAHUVUD SECTION.                                                  
001154                                                                          
001155     MOVE RUBRIK-RAD TO WS-RAD                                            
001156     PERFORM S01-SKRIV-RAD                                                
001157     MOVE SPACE TO WS-RAD                                                 
001158                                                                          
001159     ADD  +1      TO   WS-SIDA                                            
001160     MOVE WS-SIDA TO LIST-SIDA                                            
001161     MOVE U-RUBRIK-RAD1 TO WS-RAD                                         
001162     PERFORM S01-SKRIV-RAD                                                
001163     MOVE SPACE TO WS-RAD                                                 
001164                                                                          
001165     MOVE U-RUBRIK-RAD2 TO WS-RAD                                         
001166     PERFORM S01-SKRIV-RAD                                                
001167     MOVE SPACE TO WS-RAD                                                 
001168                                                                          
001169     MOVE U-RUBRIK-RAD3 TO WS-RAD                                         
001170     PERFORM S01-SKRIV-RAD                                                
001171     MOVE SPACE TO WS-RAD                                                 
001172                                                                          
001173     MOVE U-RUBRIK-RAD4 TO WS-RAD                                         
001174     PERFORM S01-SKRIV-RAD                                                
001175     MOVE SPACE TO WS-RAD                                                 
001176                                                                          
001177     MOVE U-RUBRIK-RAD5 TO WS-RAD                                         
001178     PERFORM S01-SKRIV-RAD                                                
001179     MOVE SPACE TO WS-RAD                                                 
001180                                                                          
001181     MOVE U-RUBRIK-RAD6 TO WS-RAD                                         
001182     PERFORM S01-SKRIV-RAD                                                
001183     MOVE SPACE TO WS-RAD                                                 
001184                                                                          
001185     MOVE U-RUBRIK-RAD6A TO WS-RAD                                        
001186     PERFORM S01-SKRIV-RAD                                                
001187     MOVE SPACE TO WS-RAD                                                 
001188                                                                          
001189     MOVE U-RUBRIK-LINJE TO WS-RAD                                        
001190     PERFORM S01-SKRIV-RAD                                                
001191     MOVE SPACE TO WS-RAD                                                 
001192                                                                          
001193     MOVE U-RUBRIK-RAD6C TO WS-RAD                                        
001194     PERFORM S01-SKRIV-RAD                                                
001195     MOVE SPACE TO WS-RAD                                                 
001196                                                                          
001197     MOVE U-RUBRIK-RAD7 TO WS-RAD                                         
001198     PERFORM S01-SKRIV-RAD                                                
001199     MOVE SPACE TO WS-RAD                                                 
001200                                                                          
001201*** FIL UTSKRIFT ***                                                      
001202     IF SKRIV-RUBRIK                                                      
001203       MOVE E-RUBRIK-RAD TO UT-RAD                                        
001204       PERFORM S02-SKRIV-UT-RAD                                           
001205       MOVE SPACE TO UT-RAD                                               
001206                                                                          
001207*      MOVE WS-SIDA TO E-LIST-SIDA                                        
001208       MOVE E-U-RUBRIK-RAD1 TO UT-RAD                                     
001209       PERFORM S02-SKRIV-UT-RAD                                           
001210       MOVE SPACE TO UT-RAD                                               
001211                                                                          
001212       MOVE E-U-RUBRIK-RAD2 TO UT-RAD                                     
001213       PERFORM S02-SKRIV-UT-RAD                                           
001214       MOVE SPACE TO UT-RAD                                               
001215                                                                          
001216       MOVE E-U-RUBRIK-RAD3 TO UT-RAD                                     
001217       PERFORM S02-SKRIV-UT-RAD                                           
001218       MOVE SPACE TO UT-RAD                                               
001219                                                                          
001220       MOVE E-U-RUBRIK-RAD4 TO UT-RAD                                     
001221       PERFORM S02-SKRIV-UT-RAD                                           
001222       MOVE SPACE TO UT-RAD                                               
001223                                                                          
001224       MOVE E-U-RUBRIK-RAD5 TO UT-RAD                                     
001225       PERFORM S02-SKRIV-UT-RAD                                           
001226       MOVE SPACE TO UT-RAD                                               
001227                                                                          
001228       MOVE E-U-RUBRIK-RAD6 TO UT-RAD                                     
001229       PERFORM S02-SKRIV-UT-RAD                                           
001230       MOVE SPACE TO UT-RAD                                               
001231                                                                          
001232       MOVE E-U-RUBRIK-RAD6A TO UT-RAD                                    
001233       PERFORM S02-SKRIV-UT-RAD                                           
001234       MOVE SPACE TO UT-RAD                                               
001235                                                                          
001236       MOVE E-U-RUBRIK-LINJE TO UT-RAD                                    
001237       PERFORM S02-SKRIV-UT-RAD                                           
001238       MOVE SPACE TO UT-RAD                                               
001239                                                                          
001240       MOVE E-U-RUBRIK-RAD6C TO UT-RAD                                    
001241       PERFORM S02-SKRIV-UT-RAD                                           
001242       MOVE SPACE TO UT-RAD                                               
001243                                                                          
001244       MOVE E-U-RUBRIK-RAD7 TO UT-RAD                                     
001245       PERFORM S02-SKRIV-UT-RAD                                           
001246       MOVE SPACE TO UT-RAD                                               
001247       MOVE NEJ TO SKRIV-RUBRIK-SW                                        
001248     END-IF                                                               
001249*** FIL UTSKRIFT SLUT ***                                                 
001250     .                                                                    
001251     EJECT                                                                
001252 D-BEHANDLA-RAD SECTION.                                                  
001253     SKIP2                                                                
001254     IF H3166-3166-IDDISTR = 9927 OR 9993                                 
001255        IF NDC                                                            
001256           IF NDC-US                                                      
001257              MOVE H3166-3166-IDDISTR TO PRIS-IDDISTR                     
001258              MOVE 1                  TO PRIS-IDKUNDNR                    
001259                                                                          
001260           ELSE                                                           
001261              IF NDC-CA                                                   
001262                MOVE H3166-3166-IDDISTR TO PRIS-IDDISTR                   
001263                MOVE 2                  TO PRIS-IDKUNDNR                  
001264              ELSE                                                        
001265                IF NDC-KR OR NDC-MY OR NDC-TH OR NDC-TW OR                
001266                   NDC-CN-71                                              
001267                  MOVE H3166-3166-IDDISTR TO PRIS-IDDISTR                 
001268                  MOVE WS-IDDC            TO PRIS-IDKUNDNR                
001269                END-IF                                                    
001270              END-IF                                                      
001271           END-IF                                                         
001272        ELSE                                                              
001273          MOVE H3166-3166-IDDISTR     TO PRIS-IDDISTR                     
001274          MOVE 0                      TO PRIS-IDKUNDNR                    
001275        END-IF                                                            
001276     ELSE                                                                 
001277        MOVE H3166-3166-IDDISTR       TO PRIS-IDDISTR                     
001278        MOVE 0                        TO PRIS-IDKUNDNR                    
001279     END-IF                                                               
001280     PERFORM DA-HAEMTA-PRIS-TILL-FAKTURA                                  
001281     PERFORM DB-HAEMTA-PRIS-EKO-TRANS                                     
001282     PERFORM DC-HAEMTA-ARTBEN                                             
001283     PERFORM IMS-GET-ARTC01                                               
001284     PERFORM IMS-GET-ARTC11                                               
001285     MOVE ARTC11-CLAG-VKART              TO WS-VKART                      
001286     IF NDC-NA                                                            
001287       COMPUTE WS-VKART-UNIT ROUNDED = WS-VKART * CONV-GR-TO-LB           
001288     ELSE                                                                 
001289       COMPUTE WS-VKART-UNIT ROUNDED = WS-VKART / 1000                    
001290     END-IF                                                               
001291     COMPUTE WS-VKART-TOT ROUNDED =                                       
001292             WS-VKART-UNIT * H3168-3168-KVLEVART                          
001293     COMPUTE WS-SUVKART-TOT =                                             
001294             WS-SUVKART-TOT + WS-VKART-TOT                                
001295     MOVE WS-VKART-UNIT TO LIST-VKART-UNIT                                
001296                           E-LIST-VKART-UNIT                              
001297     MOVE WS-VKART-TOT  TO LIST-VKART-TOT                                 
001298                           E-LIST-VKART-TOT                               
001299******************************************************************        
001300     MOVE ARTC11-CLAG-KDARTURS TO ARTU-KDARTURS                           
001301     MOVE SPAR-IDDISTR      TO ARTU-IDDISTR                               
001302     MOVE WS-IDDC           TO ARTU-IDDC                                  
001303     CALL W400ARTU    USING ARTU-W400ARTU                                 
001304     MOVE ARTU-KDARTURS     TO                                            
001305                              LIST-KDARTURS                               
001306                              E-LIST-KDARTURS                             
001307******************************************************************        
001308     MOVE H3168-3168-IDARTNR-OBJ          TO LIST-IDARTNR                 
001309                                             E-LIST-IDARTNR               
001310     MOVE H3168-3168-IDBYTKOL             TO LIST-IDKOLLI                 
001311                                             E-LIST-IDKOLLI               
001312     MOVE H3168-3168-KVLEVART             TO LIST-KVLEVART                
001313                                             E-LIST-KVLEVART              
001314                                                                          
001315     IF SPAR-IDBYTKOL NOT = H3168-3168-IDBYTKOL                           
001316        ADD 1                    TO WS-IDBYTKOL-TOT                       
001317        MOVE H3168-3168-IDBYTKOL TO SPAR-IDBYTKOL                         
001318     END-IF                                                               
001319                                                                          
001320     IF SKRIV-BYTES-RAPPORT = JA                                          
001321       MOVE H3168-3168-IDARTNR-OBJ        TO BYTES-IDARTNR-OBJ            
001322       MOVE H3168-3168-KVLEVART           TO BYTES-KVRETUR-URSP           
001323                                                                          
001324* WS-IDBYTFAK OCH WS-IDBYTKOLL BILDAR WS-IBYTRAP                          
001325       MOVE H3168-3168-IDBYTKOL           TO WS-IDBYTKOL                  
001326       MOVE WS-IDBYTRAP                   TO WS-IDBYTRAP-2                
001327       MOVE WS-IDBYTRAP-2                 TO BYTES-IDBYTRAP               
001328                                                                          
001329       PERFORM S13-SKRIV-W3717H                                           
001330     END-IF                                                               
001331     IF NDC-NA                                                            
001332        PERFORM DD-SKAPA-A15-TRANS                                        
001333        PERFORM S11-ISRT-WDR8-LAB                                         
001334     ELSE                                                                 
001335        PERFORM DE-SKAPA-201-201-DET                                      
001336        PERFORM S12-ISRT-WDR8                                             
001337     END-IF                                                               
001338     MOVE U-RAD TO WS-RAD                                                 
001339     PERFORM S01-SKRIV-RAD                                                
001340     MOVE SPACE TO WS-RAD                                                 
001341                                                                          
001342     MOVE E-U-RAD TO UT-RAD                                               
001343     PERFORM S02-SKRIV-UT-RAD                                             
001344     MOVE SPACE TO UT-RAD                                                 
001345     .                                                                    
001346     EJECT                                                                
001347 DA-HAEMTA-PRIS-TILL-FAKTURA SECTION.                                     
001348                                                                          
001349                                                                          
001350     MOVE 1                              TO PRIS-KDCALL                   
001351     MOVE IDPGM                          TO PRIS-IDPGM                    
001352     MOVE H3168-3168-IDARTNR-OBJ         TO PRIS-IDARTNR                  
001353     MOVE WS-IDDC                        TO PRIS-IDDC                     
001354     MOVE +4                             TO PRIS-KDORDKL                  
001355     MOVE +1                             TO PRIS-KVBEART                  
001356     MOVE SPACE                          TO PRIS-FLINVEST                 
001357                                                                          
001358     CALL W335PRIS USING PRIS-AREA                                        
001359                    PRIS-ARTC-PCB                                         
001360                    PRIS-WDK7-PCB                                         
001361                    PRIS-GMTA-PCB                                         
001362                    PRIS-BETA-PCB                                         
001363                    PRIS-GPRIA-PCB                                        
001364                    PRIS-GPRIB-PCB                                        
001365                    PRIS-COST-WDK6-PCB                                    
001366                    PRIS-COST-WDK7-PCB                                    
001367                    PRIS-COST-WDF1-PCB                                    
001368                    PRIS-COST-9305-PCB                                    
001369                    PRIS-COST-WDK72-PCB                                   
001370                    PRIS-COST-WDB6-PCB                                    
001371                                                                          
001372     IF NDC-NA                                                            
001373        PERFORM DAA-OMV-TILL-LOC-VALUTA                                   
001374     ELSE                                                                 
001375        MOVE PRIS-PRAVCOST TO WS-PRFKTUTL                                 
001376        MOVE PRIS-KDVALISO TO LIST-KDVALISO                               
001377                              E-LIST-KDVALISO                             
001378     END-IF                                                               
001379                                                                          
001380     MOVE WS-PRFKTUTL                  TO LIST-PRFKTUTL                   
001381                                          E-LIST-PRFKTUTL                 
001382     COMPUTE WS-PRFKTUTL-RAD = WS-PRFKTUTL *                              
001383                                 H3168-3168-KVLEVART                      
001384     ADD H3168-3168-KVLEVART           TO WS-KVLEVART-TOT                 
001385                                                                          
001386                                                                          
001387     MOVE WS-PRFKTUTL-RAD              TO LIST-PRFKTUTL-RAD               
001388                                          E-LIST-PRFKTUTL-RAD             
001389     ADD  WS-PRFKTUTL-RAD              TO WS-SUFKTUTL-TOT                 
001390     .                                                                    
001391     EJECT                                                                
001392 DAA-OMV-TILL-LOC-VALUTA SECTION.                                         
001393                                                                          
001394****** W335PRIS GER TILLBAKA SEK FÖR BERÖRDA           ****               
001395****** USA OCH CANADA DISTRIKT.                        ****               
001396****** SÅ HÅRDKODNINGEN NEDAN SKALL FINNAS KVAR.       ****               
001397     IF NDC-CA                                                            
001398       MOVE 'CAD'                    TO CURR-KDVALISO-ROW                 
001399                                        LIST-KDVALISO                     
001400                                        E-LIST-KDVALISO                   
001401     ELSE                                                                 
001402       IF NDC-US                                                          
001403         MOVE 'USD'                  TO CURR-KDVALISO-ROW                 
001404                                        LIST-KDVALISO                     
001405                                        E-LIST-KDVALISO                   
001406       END-IF                                                             
001407     END-IF                                                               
001408                                                                          
001409                                                                          
001410     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
001411     IF CURR-KDSVAR = ' '                                                 
001412       MOVE CURR-PRKURS-NEW          TO WS-PRKURS                         
001413     ELSE                                                                 
001414       MOVE +1                       TO WS-PRKURS                         
001415     END-IF                                                               
001416                                                                          
001417     COMPUTE WS-PRFKTUTL ROUNDED = PRIS-PRARTNTO / WS-PRKURS              
001418     IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACES                        
001419       MOVE PRIS-KDVALISO            TO CURR-KDVALISO-ROW                 
001420       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
001421       IF CURR-KDSVAR = ' '                                               
001422         CONTINUE                                                         
001423       ELSE                                                               
001424         MOVE 1                      TO CURR-PRKURS-NEW                   
001425       END-IF                                                             
001426       COMPUTE WS-PRFKTUTL ROUNDED =                                      
001427                  PRIS-PRARTNTO * WS-PRKURS / CURR-PRKURS-NEW             
001428     END-IF                                                               
001429     .                                                                    
001430     EJECT                                                                
001431 DB-HAEMTA-PRIS-EKO-TRANS    SECTION.                                     
001432                                                                          
001433                                                                          
001434     MOVE H3168-3168-IDARTNR-OBJ         TO W-IDARTNR                     
001435     MOVE WS-IDDC                        TO W-IDDC                        
001436     MOVE H3168-3168-KVLEVART            TO PRIS-KVBEART                  
001437     PERFORM IMS-GET-ARTS-ARTS11                                          
001438     IF SEGMENT-FINNS                                                     
001439                                                                          
001440       MOVE  ARTS11-SLAG-PRAVCOST TO WS-PRAVCOST                          
001441     ELSE                                                                 
001442       DISPLAY 'ARTIKEL ' H3168-3168-IDARTNR-OBJ ' SAKNAS PÅ WDK7'        
001443       CALL ABEND                                                         
001444     END-IF                                                               
001445     .                                                                    
001446     EJECT                                                                
001447 DC-HAEMTA-ARTBEN SECTION.                                                
001448     SKIP2                                                                
001449     MOVE H3168-3168-IDARTNR-OBJ TO W-IDARTNR                             
001450     MOVE 'GB'              TO W-IDSKYLT                                  
001451     PERFORM IMS-GET-ART-BEN                                              
001452     IF SEGMENT-FINNS                                                     
001453       MOVE BENA-TEXT-BEART TO LIST-BEART                                 
001454                               E-LIST-BEART                               
001455     ELSE                                                                 
001456       MOVE SPACE           TO LIST-BEART                                 
001457                               E-LIST-BEART                               
001458     END-IF                                                               
001459     .                                                                    
001460     EJECT                                                                
001461 DD-SKAPA-A15-TRANS SECTION.                                              
001462     MOVE ZERO                   TO A15-KDPRODSL                          
001463                                    A15-KDPSLLOC                          
001464     MOVE H3166-3166-DAFAKT      TO A15-DAFAKT                            
001465     MOVE ZERO                   TO A15-IDKUNDNR                          
001466     MOVE 'A15'                  TO A15-IDPTYP                            
001467     MOVE H3166-3166-IDDC        TO A15-IDDC-SEND                         
001468     MOVE H3166-3166-IDDISTR     TO A15-IDDISTR                           
001469     IF H3166-3166-IDDISTR = 9927                                         
001470       MOVE 'O31'                TO A15-KDEKOHT                           
001471       MOVE WC-SDC-NL-ET         TO A15-IDDC-REC                          
001472     ELSE                                                                 
001473       MOVE 'O32'                TO A15-KDEKOHT                           
001474       MOVE H3166-3166-IDDC      TO A15-IDDC-REC                          
001475     END-IF                                                               
001476     MOVE H3166-3166-IDBYTFAK    TO A15-IDFAKT                            
001477     IF NDC-CA                                                            
001478       MOVE 54                   TO A15-IDFTG                             
001479     ELSE                                                                 
001480       IF NDC-US                                                          
001481          MOVE 53                TO A15-IDFTG                             
001482       END-IF                                                             
001483     END-IF                                                               
001484     MOVE ARTC-ART-KDPRODSL      TO A15-KDPRODSL                          
001485     MOVE ARTC11-CLAG-KDPSLLOC   TO A15-KDPSLLOC                          
001486     MOVE H3168-3168-IDARTNR-OBJ TO A15-IDARTNR                           
001487     MOVE H3168-3168-KVLEVART    TO A15-KVLEVART                          
001488     MOVE WS-PRAVCOST            TO A15-PRAVCOST                          
001489                                                                          
001490     SKIP2                                                                
001491     .                                                                    
001492     EJECT                                                                
001493 DE-SKAPA-201-201-DET SECTION.                                            
001494     MOVE '201'                  TO EKH-KDEKHHT                           
001495     MOVE '201'                  TO EKH-KDEKSHT                           
001496     MOVE 'DET'                  TO EKH-KDEKNIVA                          
001497     MOVE H3166-3166-IDDC        TO EKH-IDDC-SEND                         
001498     MOVE WC-SDC-NL-ET           TO EKH-IDDC-REC                          
001499     MOVE H3166-3166-IDDISTR     TO EKH-IDDISTR                           
001500     MOVE ZERO                   TO EKH-IDKUNDNR                          
001501                                                                          
001502     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
001503     MOVE H3166-3166-IDBYTFAK    TO CIA-IDARTBET-IN                       
001504     CALL W009CIA USING             CIA-W009CIA                           
001505     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
001506                                                                          
001507     MOVE H3166-3166-DAFAKT      TO EKH-DAVERDAT                          
001508     MOVE ARTC-ART-KDPRODSL      TO EKH-KDPRODSL                          
001509     MOVE ARTC11-CLAG-KDPSLLOC   TO EKH-KDPSLLOC                          
001510     MOVE ARTC-ART-KDSORT        TO EKH-KDSORT                            
001511     MOVE H3168-3168-IDARTNR-OBJ TO EKH-IDARTNR                           
001512     MOVE SPACE                  TO EKH-FLLSBOK                           
001513     IF NDC-KR                                                            
001514       MOVE 'KRW'                TO EKH-KDVALISO                          
001515     END-IF                                                               
001516     MOVE  1.00                  TO EKH-PRKURS                            
001517     MOVE ZERO                   TO EKH-PRARTNTO                          
001518     MOVE ZERO                   TO EKH-PRARTSJK                          
001519     MOVE ZERO                   TO EKH-PRHEMTAG                          
001520     MOVE WS-PRAVCOST            TO EKH-PRARTSTD                          
001521     MOVE ZERO                   TO EKH-PRLANDCO                          
001522     MOVE ZERO                   TO EKH-PRINK                             
001523     MOVE ZERO                   TO EKH-PRDIRLON                          
001524     MOVE ZERO                   TO EKH-PRDMTRL                           
001525     MOVE ZERO                   TO EKH-PROVRPAL                          
001526     MOVE H3168-3168-KVLEVART    TO EKH-KVANTAL                           
001527     MOVE ZERO                   TO EKH-SUBEL                             
001528     MOVE '    '                 TO EKH-IDTRANS                           
001529     MOVE ZERO                   TO EKH-BEVAT                             
001530                                    EKH-IDANALYS                          
001531                                    EKH-KDANMORS                          
001532                                    EKH-KDFRAKT                           
001533                                    EKH-SUVAT                             
001534                                    EKH-DAAVIDAT                          
001535                                    EKH-IDAVINR                           
001536                                    EKH-KDAVVTYP                          
001537                                    EKH-KDRT                              
001538                                    EKH-KVANTMOT                          
001539                                    EKH-KVAVIS                            
001540                                    EKH-IDORDNR5                          
001541     MOVE SPACE                  TO EKH-IDLEVNR                           
001542                                    EKH-IDUSER                            
001543                                    EKH-IDREF                             
001544                                    EKH-BEFELSAP                          
001545     MOVE SPACE                  TO EKH-FLOVRLEV                          
001546                                    EKH-FLDCET                            
001547                                    EKH-FLKLAR                            
001548     MOVE SPACE                  TO EKH-IDKUNDRF                          
001549     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
001550     IF NDC-KR                                                            
001551       MOVE 0000546001           TO EKH-IDKONTO                           
001552       MOVE 'HC30000'            TO EKH-IDKST                             
001553       MOVE 'KR02'               TO EKH-KDTRADP                           
001554       MOVE 'KR02EKHA'           TO FIL-IDCPYTXT IN FIL-WDR801            
001555     END-IF                                                               
001556     IF NDC-MY                                                            
001557       MOVE 0000633401           TO EKH-IDKONTO                           
001558       MOVE 'HC30700'            TO EKH-IDKST                             
001559       MOVE 'MY04'               TO EKH-KDTRADP                           
001560       MOVE 'MY04EKHA'           TO FIL-IDCPYTXT IN FIL-WDR801            
001561       MOVE 'MYR'                TO EKH-KDVALISO                          
001562     END-IF                                                               
001563     IF NDC-TH                                                            
001564*** CREATE 201-202 FOR THAILAND (FREE OF CHARGE TO CORES REMAN)           
001565       MOVE '201'                TO EKH-KDEKHHT                           
001566       MOVE '202'                TO EKH-KDEKSHT                           
001567       MOVE 0000289902           TO EKH-IDKONTO                           
001568       MOVE 'HI30130'            TO EKH-IDKST                             
001569       MOVE 'TH01'               TO EKH-KDTRADP                           
001570       MOVE 'TH01EKHA'           TO FIL-IDCPYTXT IN FIL-WDR801            
001571       MOVE 'THB'                TO EKH-KDVALISO                          
001572     END-IF                                                               
001573     IF NDC-TW                                                            
001574*** CREATE 201-202 FOR TAIWAN (FREE OF CHARGE TO CORES REMAN)             
001575       MOVE '201'                TO EKH-KDEKHHT                           
001576       MOVE '202'                TO EKH-KDEKSHT                           
001577       MOVE 0000482301           TO EKH-IDKONTO                           
001578       MOVE 'HH30900'            TO EKH-IDKST                             
001579       MOVE 'TW01'               TO EKH-KDTRADP                           
001580       MOVE 'TW01EKHA'           TO FIL-IDCPYTXT IN FIL-WDR801            
001581       MOVE 'TWD'                TO EKH-KDVALISO                          
001582     END-IF                                                               
001583     IF NDC-CN-71                                                         
001584*** CREATE 201-202 FOR CHINA  (FREE OF CHARGE TO CORES REMAN)             
001585       MOVE '201'                TO EKH-KDEKHHT                           
001586       MOVE '202'                TO EKH-KDEKSHT                           
001587       MOVE 0000483104           TO EKH-IDKONTO                           
001588       MOVE SPACE                TO EKH-IDKST                             
001589       MOVE 'CN05'               TO EKH-KDTRADP                           
001590       MOVE 'W570EKHA'           TO FIL-IDCPYTXT IN FIL-WDR801            
001591       MOVE 'CNY'                TO EKH-KDVALISO                          
001592     END-IF                                                               
001593     MOVE EKH-W510EKHA         TO FIL-WDR801-DATA                         
001594     SKIP2                                                                
001595     .                                                                    
001596     EJECT                                                                
001597 F-SKAPA-SLUTSIDA SECTION.                                                
001598     SKIP2                                                                
001599     MOVE WS-IDBYTKOL-TOT  TO LIST-IDBYTKOL-TOT                           
001600                              E-LIST-IDBYTKOL-TOT                         
001601     MOVE WS-SUFKTUTL-TOT  TO LIST-SUFKTUTL-TOT                           
001602                              E-LIST-SUFKTUTL-TOT                         
001603     MOVE WS-SUVKART-TOT   TO LIST-SUVKART-TOT                            
001604                              E-LIST-SUVKART-TOT                          
001605     IF NDC-NA                                                            
001606       MOVE 'LBS'            TO LIST-UOM-TWEIGHT                          
001607       MOVE ' LBS'           TO E-LIST-UOM-TWEIGHT                        
001608     ELSE                                                                 
001609       MOVE 'KG '            TO LIST-UOM-TWEIGHT                          
001610       MOVE ' KG '           TO E-LIST-UOM-TWEIGHT                        
001611     END-IF                                                               
001612                                                                          
001613     MOVE E-U-RUBRIK-LINJE TO UT-RAD                                      
001614     PERFORM S02-SKRIV-UT-RAD                                             
001615     MOVE SPACE TO UT-RAD                                                 
001616                                                                          
001617     MOVE SISTA-RADEN1    TO WS-RAD                                       
001618     MOVE E-SISTA-RADEN1  TO UT-RAD                                       
001619     PERFORM S01-SKRIV-RAD                                                
001620     PERFORM S02-SKRIV-UT-RAD                                             
001621     MOVE SPACE TO WS-RAD                                                 
001622                   UT-RAD                                                 
001623                                                                          
001624     MOVE SISTA-RADEN2    TO WS-RAD                                       
001625     MOVE E-SISTA-RADEN2  TO UT-RAD                                       
001626     PERFORM S01-SKRIV-RAD                                                
001627     PERFORM S02-SKRIV-UT-RAD                                             
001628     MOVE SPACE TO WS-RAD                                                 
001629                   UT-RAD                                                 
001630                                                                          
001631     MOVE SISTA-RADEN3   TO WS-RAD                                        
001632     MOVE E-SISTA-RADEN3 TO UT-RAD                                        
001633     PERFORM S01-SKRIV-RAD                                                
001634     PERFORM S02-SKRIV-UT-RAD                                             
001635     MOVE SPACE          TO WS-RAD                                        
001636                            UT-RAD                                        
001637                                                                          
001638     MOVE SISTA-RADEN4    TO WS-RAD                                       
001639     MOVE E-SISTA-RADEN4  TO UT-RAD                                       
001640     PERFORM S01-SKRIV-RAD                                                
001641     PERFORM S02-SKRIV-UT-RAD                                             
001642     MOVE SPACE           TO WS-RAD                                       
001643                             UT-RAD                                       
001644                                                                          
001645     MOVE SISTA-RADEN5    TO WS-RAD                                       
001646     MOVE E-SISTA-RADEN5  TO UT-RAD                                       
001647     PERFORM S01-SKRIV-RAD                                                
001648     PERFORM S02-SKRIV-UT-RAD                                             
001649     MOVE SPACE           TO WS-RAD                                       
001650                             UT-RAD                                       
001651                                                                          
001652     MOVE SISTA-RADEN6    TO WS-RAD                                       
001653     MOVE E-SISTA-RADEN6  TO UT-RAD                                       
001654     PERFORM S01-SKRIV-RAD                                                
001655     PERFORM S02-SKRIV-UT-RAD                                             
001656     MOVE SPACE           TO WS-RAD                                       
001657                          UT-RAD                                          
001658     .                                                                    
001659     EJECT                                                                
001660 Z-FINIT SECTION.                                                         
001661                                                                          
001662     SKIP2                                                                
001663     IF FL-WEBDC                                                          
001664        PERFORM S03-SKRIV-UT2-RAD                                         
001665     END-IF                                                               
001666     CLOSE W3717H                                                         
001667     CLOSE W37178                                                         
001668     CLOSE W3717C                                                         
001669     MOVE 'S' TO POSTSUM-OPKOD                                            
001670     CALL POSTSUM USING POSTSUM-PARM                                      
001671     .                                                                    
001672     EJECT                                                                
001673 S11-ISRT-WDR8-LAB SECTION.                                               
001674     SKIP2                                                                
001675     MOVE IDPGM                TO FIL-IDPGM    IN FIL-WDR801              
001676     MOVE FUNCTION CURRENT-DATE (3:6)                                     
001677                               TO FIL-TIREGDAT IN FIL-WDR801              
001678     MOVE FUNCTION CURRENT-DATE (9:7)                                     
001679                               TO FIL-TIKLOCK  IN FIL-WDR801              
001680     ADD +1                    TO FIL-TIKLOCK  IN FIL-WDR801              
001681     MOVE +1                   TO FIL-IDSEKVNR IN FIL-WDR801              
001682     MOVE 'W510A15 '           TO FIL-IDCPYTXT IN FIL-WDR801              
001683     MOVE A15-W510A15          TO FIL-WDR801-DATA                         
001684                                                                          
001685     PERFORM IMS-ISRT-WDR801                                              
001686     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
001687         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
001688         PERFORM IMS-ISRT-WDR801                                          
001689     END-PERFORM                                                          
001690     .                                                                    
001691     EJECT                                                                
001692 S12-ISRT-WDR8 SECTION.                                                   
001693     SKIP2                                                                
001694     MOVE IDPGM                TO FIL-IDPGM    IN FIL-WDR801              
001695     MOVE FUNCTION CURRENT-DATE (3:6)                                     
001696                               TO FIL-TIREGDAT IN FIL-WDR801              
001697     MOVE FUNCTION CURRENT-DATE (9:7)                                     
001698                               TO FIL-TIKLOCK  IN FIL-WDR801              
001699     ADD +1                    TO FIL-TIKLOCK  IN FIL-WDR801              
001700     MOVE +1                   TO FIL-IDSEKVNR IN FIL-WDR801              
001701                                                                          
001702     PERFORM IMS-ISRT-WDR801                                              
001703     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
001704         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
001705         PERFORM IMS-ISRT-WDR801                                          
001706     END-PERFORM                                                          
001707     .                                                                    
001708     EJECT                                                                
001709 S13-SKRIV-W3717H SECTION.                                                
001710     SKIP2                                                                
001711     WRITE BYTES-POST FROM BYTES-AREA                                     
001712                                                                          
001713     MOVE BYTES-IDPTYP TO POSTSUM-TRANSTYP                                
001714     MOVE 'W3717H ' TO POSTSUM-FDNAMN                                     
001715     MOVE 'W37178D2' TO POSTSUM-DDNAMN2                                   
001716     CALL POSTSUM USING POSTSUM-PARM                                      
001717     .                                                                    
001718     EJECT                                                                
001719 S01-SKRIV-RAD   SECTION.                                                 
001720     SKIP2                                                                
001721     IF FL-WEBDC                                                          
001722        PERFORM S03-SKRIV-UT2-RAD                                         
001723     END-IF                                                               
001724     .                                                                    
001725 S02-SKRIV-UT-RAD   SECTION.                                              
001726     IF H3166-3166-IDDISTR = 9927                                         
001727       WRITE UTPOST FROM UT-RAD                                           
001728       MOVE 'UT-'        TO POSTSUM-TRANSTYP                              
001729       MOVE 'W37178 ' TO POSTSUM-FDNAMN                                   
001730       MOVE 'W37178D3' TO POSTSUM-DDNAMN2                                 
001731       CALL POSTSUM USING POSTSUM-PARM                                    
001732     END-IF                                                               
001733     .                                                                    
001734 S03-SKRIV-UT2-RAD  SECTION.                                              
001735     WRITE UTPOST2 FROM WS-RAD                                            
001736     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
001737     MOVE 'W3717C '  TO POSTSUM-FDNAMN                                    
001738     MOVE 'W37178D4' TO POSTSUM-DDNAMN2                                   
001739     CALL POSTSUM USING POSTSUM-PARM                                      
001740     .                                                                    
001741     EJECT                                                                
001742* --- IMS SEKTIONER ---                                                   
001743     SKIP3                                                                
001744     EJECT                                                                
001745 IMS-GET-GMTA SECTION.                                                    
001746     MOVE 'IMS-GET-GMTA' TO WS-IMS-SEKTION                                
001747                                                                          
001748     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
001749                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
001750          DELIMITED BY SIZE INTO SSA1                                     
001751     MOVE '  GE' TO GODK-STATUSKODER                                      
001752     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
001753     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
001754     PERFORM IMS-STATUSKONTROLL                                           
001755     .                                                                    
001756     EJECT                                                                
001757 IMS-GET-ARTC01 SECTION.                                                  
001758     MOVE 'IMS-GET-ARTC01' TO WS-IMS-SEKTION                              
001759                                                                          
001760     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001761          DELIMITED BY SIZE INTO SSA1                                     
001762     MOVE '  '   TO GODK-STATUSKODER                                      
001763     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
001764     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001765     PERFORM IMS-STATUSKONTROLL                                           
001766     .                                                                    
001767     EJECT                                                                
001768 IMS-GET-ARTC11 SECTION.                                                  
001769     MOVE 'IMS-GET-ARTC11' TO WS-IMS-SEKTION                              
001770                                                                          
001771     SKIP2                                                                
001772     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001773           DELIMITED BY SIZE INTO SSA1                                    
001774     MOVE 'WLARTC11 ' TO SSA2                                             
001775     MOVE '  ' TO GODK-STATUSKODER                                        
001776     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
001777     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001778     PERFORM IMS-STATUSKONTROLL                                           
001779     .                                                                    
001780     EJECT                                                                
001781 IMS-GET-ARTS-ARTS11 SECTION.                                             
001782     MOVE 'IMS-GET-ARTS11' TO WS-IMS-SEKTION                              
001783                                                                          
001784     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
001785          DELIMITED BY SIZE INTO SSA1                                     
001786     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
001787          DELIMITED BY SIZE INTO SSA2                                     
001788     MOVE '  GE' TO GODK-STATUSKODER                                      
001789     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-WLARTS11  SSA1 SSA2           
001790     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
001791     PERFORM IMS-STATUSKONTROLL                                           
001792     .                                                                    
001793     EJECT                                                                
001794 IMS-GET-3165-H3165 SECTION.                                              
001795     MOVE 'IMS-GET-3165-H3165' TO WS-IMS-SEKTION                          
001796                                                                          
001797     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
001798          DELIMITED BY SIZE INTO SSA1                                     
001799     MOVE '  GE' TO GODK-STATUSKODER                                      
001800     CALL CBLTDLI USING GU 3165-PCB DLI-IO-WL316501 SSA1                  
001801     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001802     PERFORM IMS-STATUSKONTROLL                                           
001803     .                                                                    
001804     EJECT                                                                
001805 IMS-GET-3165-H3166 SECTION.                                              
001806     MOVE 'IMS-GET-3165-H3166' TO WS-IMS-SEKTION                          
001807                                                                          
001808     STRING 'WL316511(IDBYTFAK=>' W-IDBYTFAK-X ')'                        
001809          DELIMITED BY SIZE INTO SSA1                                     
001810     MOVE '  GE' TO GODK-STATUSKODER                                      
001811     CALL CBLTDLI USING GHNP 3165-PCB DLI-IO-WL316511 SSA1                
001812     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001813     PERFORM IMS-STATUSKONTROLL                                           
001814     .                                                                    
001815     EJECT                                                                
001816 IMS-GET-3165-H3168-FIRST SECTION.                                        
001817     MOVE 'IMS-GET-3165-H3168-FIRST' TO WS-IMS-SEKTION                    
001818                                                                          
001819     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
001820          DELIMITED BY SIZE INTO SSA1                                     
001821     MOVE 'WL316521*F' TO SSA2                                            
001822     MOVE '  GE' TO GODK-STATUSKODER                                      
001823     CALL CBLTDLI USING GHNP 3165-PCB DLI-IO-WL316521 SSA1 SSA2           
001824     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001825     PERFORM IMS-STATUSKONTROLL                                           
001826     .                                                                    
001827     SKIP3                                                                
001828 IMS-GET-3165-H3168 SECTION.                                              
001829     MOVE 'IMS-GET-3165-H3168'      TO WS-IMS-SEKTION                     
001830                                                                          
001831     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
001832          DELIMITED BY SIZE INTO SSA1                                     
001833     MOVE 'WL316521 ' TO SSA2                                             
001834     MOVE '  GE' TO GODK-STATUSKODER                                      
001835     CALL CBLTDLI USING GHNP 3165-PCB DLI-IO-WL316521 SSA1 SSA2           
001836     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001837     PERFORM IMS-STATUSKONTROLL                                           
001838     .                                                                    
001839     SKIP3                                                                
001840 IMS-GET-UNIK-H3166 SECTION.                                              
001841     MOVE 'IMS-GET-UNIK-3165'      TO WS-IMS-SEKTION                      
001842                                                                          
001843     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
001844          DELIMITED BY SIZE INTO SSA1                                     
001845     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
001846          DELIMITED BY SIZE INTO SSA2                                     
001847     MOVE '  GE' TO GODK-STATUSKODER                                      
001848     CALL CBLTDLI USING GHU 3165-PCB DLI-IO-WL316511 SSA1 SSA2            
001849     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001850     PERFORM IMS-STATUSKONTROLL                                           
001851     .                                                                    
001852     EJECT                                                                
001853 IMS-DLET-3165 SECTION.                                                   
001854     MOVE 'IMS-DLET-3165'      TO WS-IMS-SEKTION                          
001855                                                                          
001856     MOVE '  ' TO GODK-STATUSKODER                                        
001857     CALL CBLTDLI USING DLET 3165-PCB DLI-IO-WL316511                     
001858     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
001859     PERFORM IMS-STATUSKONTROLL                                           
001860     .                                                                    
001861     EJECT                                                                
001862 IMS-GET-ART-BEN SECTION.                                                 
001863     MOVE 'IMS-GET-ART-BEN'    TO WS-IMS-SEKTION                          
001864     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
001865          DELIMITED BY SIZE INTO SSA1                                     
001866     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
001867          DELIMITED BY SIZE INTO SSA2                                     
001868     MOVE '  GE' TO GODK-STATUSKODER                                      
001869     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
001870     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001871     PERFORM IMS-STATUSKONTROLL                                           
001872     .                                                                    
001873 IMS-ISRT-WDR801   SECTION.                                               
001874     MOVE 'WDR801   '      TO SSA1                                        
001875     MOVE '  II'           TO GODK-STATUSKODER                            
001876     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
001877     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
001878     PERFORM IMS-STATUSKONTROLL                                           
001879     .                                                                    
001880     EJECT                                                                
001881 IMS-STATUSKONTROLL SECTION.                                              
001882     SKIP2                                                                
001883     SET STATUS-IX TO 1                                                   
001884     SEARCH GODK-STATUS                                                   
001885       AT END                                                             
001886         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001887           DELIMITED BY SIZE INTO FELTEXT                                 
001888         DISPLAY FELTEXT                                                  
001889         CALL FELLOG                                                      
001890       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001891         CONTINUE                                                         
001892     END-SEARCH                                                           
001900     .                                                                    
