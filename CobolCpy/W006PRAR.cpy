000010*** EDIT ALLOWED                                                          
000200*                                    *   COPYTEXT FÖR CALL TILL           
000300*                                    *   PRINTER SUBPROGRAM.              
000400*                                                                         
001700*                                                                         
001710*  1.    PRINT COM (MED VCOM/SPOOL)      W006PRC1                         
001720*             (COMMUNICATIONSINTERFACE FÖR VCOM)                          
001730*                                                                         
001740*    EX: CALL W006PRC1 USING PRT-VCOM      PRT-WRITE PRT-IDPRTLST         
001750*                            ALT-PCB1                                     
001760*                            PRC1-W006PRVC                                
001770*                                                                         
001780*  2.    PRINT REPRINT (MED SPOOL ÅTERSTART) W006PRR1                     
001790*                                            W006PRR2                     
001791*                                                                         
001792*    EX: CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE PRT-IDPRTLST         
001793*                            ALT-PCB1 LISB-PCB1 PRT-IDLIST                
001794*                            PRT-AFTER-2 PRT-RAD                          
001795*                                                                         
001796*  3.    PRINT SPOOLOUT (UTAN ÅTERSTART)     W006PRS1                     
001797*                                            W006PRS2                     
001798*                                                                         
001799*    EX: CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE PRT-IDPRTLST         
001800*                            ALT-PCB1                                     
001801*                            PRT-AFTER-1 PRT-RAD                          
001802*                                                                         
001810 01  PRT-W006PRAR.                                                        
001900*                                    * VÄLJ CALL-PARAMETRAR               
007802**********     LIST-TYP TILL W006PRR1 OCH W006PRS1                        
007804*                                                                         
007872   03  PRT-SPOOL-A4S.                                                     
007873     05  FILLER                  PIC X(8)    VALUE 'SPO-A4S '.            
007874*                                    * NORMALLISTA 70 LINES 80 POS        
007880*                                    * START LINE 01 POS 001              
007890*                                    * STOP  LINE 70 POS 80               
007891     05  PRT-COPIES-A4S          PIC X(1)    VALUE '1'.                   
007892*                                    * ANTAL KOPIOR ATT PRINTA            
007893*                                    * 1 TILL 9                           
007894     05  PRT-FORMS-A4S           PIC X(1)    VALUE 'A'.                   
007895*                                    * FORMSNUMMER                        
007896*                                    * A = STD                            
007897*                                    * B = F001 (STD HOLLAND)             
007898*                                    * C = 4811 (STD BELGIEN)             
007899*                                    * D = VCAS (SÖKA FAKTUROR)           
007900*                                    * 2 = 2000 (LIGGANDE A4  )           
007901*                                    * 3 = 3000 (STÅENDE A4   )           
007902     05  PRT-PFDEF-A4S           PIC X(8)    VALUE SPACE.                 
007903*                                    * FORMSDEF/PAGEDEF                   
007904     05  PRT-COPYG-A4S           PIC X(8)    VALUE SPACE.                 
007905*                                    * COPYGROUP PSF IBM                  
007906     05  FILLER                  PIC X(24)   VALUE SPACE.                 
007907*                                    * FOR FUTURE USE                     
007908*                                                                         
007911     05  PRT-IDTFX-A4S           PIC X(20)   VALUE SPACE.                 
007912*                                    * TELE-FAX-NUMMER                    
007913     05  PRT-FAX-01-A4S          PIC X(50)   VALUE SPACE.                 
007914*                                    * RAD 1 FÖRSÄTTSBLAD                 
007915     05  PRT-FAX-02-A4S          PIC X(50)   VALUE SPACE.                 
007916*                                    * RAD 2 FÖRSÄTTSBLAD                 
007917     05  PRT-FAX-03-A4S          PIC X(50)   VALUE SPACE.                 
007918*                                    * RAD 3 FÖRSÄTTSBLAD                 
007919     05  PRT-FAX-04-A4S          PIC X(50)   VALUE SPACE.                 
007920*                                    * RAD 4 FÖRSÄTTSBLAD                 
007921     05  PRT-FAX-05-A4S          PIC X(50)   VALUE SPACE.                 
007922*                                    * RAD 5 FÖRSÄTTSBLAD                 
007923*                                                                         
007924*                                                                         
007925   03  PRT-SPOOL-OVR.                                                     
007926     05 FILLER                   PIC X(8)    VALUE 'SPO-OVR '.            
007927*                                    * ÖVRIGA LISTOR                      
007928*                                    * MAX 48 LINES 132 POS               
007929*                                    * START LINE 01 POS 001              
007930*                                    * STOP  LINE 48 POS 132              
007931*                                    * SIDBRYTNING SKÖTS AV               
007932*                                    * KALLANDE PROGRAM                   
007933     05  PRT-COPIES-OVR          PIC X(1)    VALUE '1'.                   
007934*                                    * ANTAL KOPIOR ATT PRINTA            
007935*                                    * 1 TILL 9                           
007936     05  PRT-FORMS-OVR           PIC X(1)    VALUE 'A'.                   
007937*                                    * FORMSNUMMER                        
007938*                                    * A = STD                            
007939*                                    * B = F001 (STD HOLLAND)             
007940*                                    * C = 4811 (STD BELGIEN)             
007941*                                    * D = VCAS (SÖKA FAKTUROR)           
007942*                                    * 2 = 2000 (LIGGANDE A4  )           
007943*                                    * 3 = 3000 (STÅENDE A4   )           
007944     05  PRT-PFDEF-OVR           PIC X(8)    VALUE SPACE.                 
007945*                                    * FORMSDEF/PAGEDEF                   
007946     05  PRT-COPYG-OVR           PIC X(8)    VALUE SPACE.                 
007947*                                    * COPYGROUP PSF IBM                  
007948     05  FILLER                  PIC X(24)   VALUE SPACE.                 
007949*                                    * FOR FUTURE USE                     
007950*                                                                         
007951     05  PRT-IDTFX-OVR           PIC X(20)   VALUE SPACE.                 
007952*                                    * TELE-FAX-NUMMER                    
007957     05  PRT-FAX-01-OVR          PIC X(50)   VALUE SPACE.                 
007958*                                    * RAD 1 FÖRSÄTTSBLAD                 
007959     05  PRT-FAX-02-OVR          PIC X(50)   VALUE SPACE.                 
007960*                                    * RAD 2 FÖRSÄTTSBLAD                 
007961     05  PRT-FAX-03-OVR          PIC X(50)   VALUE SPACE.                 
007962*                                    * RAD 3 FÖRSÄTTSBLAD                 
007963     05  PRT-FAX-04-OVR          PIC X(50)   VALUE SPACE.                 
007964*                                    * RAD 4 FÖRSÄTTSBLAD                 
007965     05  PRT-FAX-05-OVR          PIC X(50)   VALUE SPACE.                 
007966*                                    * RAD 5 FÖRSÄTTSBLAD                 
007967*                                                                         
007968*                                                                         
007969**********     LIST-TYP TILL W006PRC1                                     
007970*                                                                         
007971   03  PRT-VCOM.                                                          
007972*                                    * VCOM-PARAMETRAR                    
007973     05  PRT-IDVCOM              PIC X(8)    VALUE '        '.            
007974*                                    * VCOM IDENTITET                     
007975     05  PRT-IDCPYTXT            PIC X(8)    VALUE '        '.            
007976*                                    * COPYTEXT IDENTITET                 
007980**********        CALL-TYP                                                
008000*                                                                         
008100*                                                                         
008200   03  PRT-OPEN                  PIC X(5)    VALUE 'OPEN '.               
008300*                                    * AREOR STÄLLS I STARTLÄGE           
008400   03  PRT-WRITE                 PIC X(5)    VALUE 'WRITE'.               
008500*                                    * VID NYSIDA ELLER SIDAN FULL        
008600*                                    * SKRIVS FÖREGÅENDE SIDA UT,         
008700*                                    * I ÖVRIGT SPARAS RADEN.             
008800   03  PRT-PURGE                 PIC X(5)    VALUE 'PURGE'.               
008900*                                    * SIDAN SKRIVS UT OCH AREOR          
009000*                                    * STÄLLS I STARTLÄGE                 
009100*                                    * FÖR BYTE AV LISTA ELLER            
009200*                                    * FÖR CHKT-TAGNING                   
009300   03  PRT-CLOSE                 PIC X(5)    VALUE 'CLOSE'.               
009400*                                    * SIDAN SKRIVS UT                    
009500**********        LIST-IDENTITET                                          
009600*                                                                         
010000   03  PRT-IDLIST                PIC X(10)   VALUE SPACE.                 
010100*                                    * LISTIDENTITET VID ÅTERSTART        
010200*                                    * EX: FAKTURANUMMER                  
010300   03  PRT-FILLER                PIC X(10)   VALUE SPACE.                 
010400*                                    *   DUMMY-PARAMETER                  
010401   03  PRT-IDPGM                 PIC X(8)    VALUE SPACE.                 
010402*                                    * PROGRAMNAMN                        
010410**********        LIST-PARAMETRAR                                         
010420*                                                                         
010500   03  PRT-CHKP-MAX              PIC S9(3)   VALUE +25   COMP-3.          
010600*                                    * MAX ANTAL SIDOR MELLAN             
010700*                                    * CHECK-POINT                        
010710   03  PRT-RAD                   PIC X(132)  VALUE SPACE.                 
010720*                                    * RADEN SOM SKALL PRINTAS            
010800**********        RAD OCH SID SKIPP                                       
010900*                                    * STYRNING AV SIDBRYTNING            
011000*                                    * OCH RAD-SKIPP                      
011100*                                                                         
011200   03  PRT-NYSIDA-RAD1           PIC S9(3)   VALUE +901  COMP-3.          
011300*                                    * SKRIVER PÅ NY SIDA RAD 1           
011400   03  PRT-NYSIDA-RAD2           PIC S9(3)   VALUE +902  COMP-3.          
011500*                                    * SKRIVER PÅ NY SIDA RAD 2           
011600   03  PRT-NYSIDA-RAD3           PIC S9(3)   VALUE +903  COMP-3.          
011700*                                    * SKRIVER PÅ NY SIDA RAD 3           
011800   03  PRT-NYSIDA-RAD4           PIC S9(3)   VALUE +904  COMP-3.          
011900*                                    * SKRIVER PÅ NY SIDA RAD 4           
012000   03  PRT-NYSIDA-RAD5           PIC S9(3)   VALUE +905  COMP-3.          
012100*                                    * SKRIVER PÅ NY SIDA RAD 5           
012200   03  PRT-NYSIDA-RAD6           PIC S9(3)   VALUE +906  COMP-3.          
012300*                                    * SKRIVER PÅ NY SIDA RAD 6           
012400   03  PRT-NYSIDA-RAD7           PIC S9(3)   VALUE +907  COMP-3.          
012500*                                    * SKRIVER PÅ NY SIDA RAD 7           
012510   03  PRT-NYSIDA-RAD20          PIC S9(3)   VALUE +920  COMP-3.          
012520*                                    * SKRIVER PÅ NY SIDA RAD 20          
012600   03  PRT-RADSKIP               PIC S9(3)   VALUE +0    COMP-3.          
012700*                                    * VALFRITT ANTAL BLANKA RADER        
012800   03  PRT-AFTER-0               PIC S9(3)   VALUE +0    COMP-3.          
012900*                                    * SKRIV PÅ SAMMA RAD                 
013000   03  PRT-AFTER-1               PIC S9(3)   VALUE +1    COMP-3.          
013100*                                    * SKRIV PÅ NÄSTA RAD                 
013200   03  PRT-AFTER-2               PIC S9(3)   VALUE +2    COMP-3.          
013300*                                    * 1 BLANK RAD EMELLAN                
013400   03  PRT-AFTER-3               PIC S9(3)   VALUE +3    COMP-3.          
013500*                                    * 2 BLANKA RADER                     
013600   03  PRT-AFTER-4               PIC S9(3)   VALUE +4    COMP-3.          
013700*                                    * 3 BLANKA RADER                     
013800   03  PRT-AFTER-5               PIC S9(3)   VALUE +5    COMP-3.          
013900*                                    * 4 BLANKA RADER                     
014000   03  PRT-AFTER-6               PIC S9(3)   VALUE +6    COMP-3.          
014100*                                    * 5 BLANKA RADER                     
014200   03  PRT-AFTER-7               PIC S9(3)   VALUE +7    COMP-3.          
014300*                                    * 6 BLANKA RADER                     
014400   03  PRT-AFTER-8               PIC S9(3)   VALUE +8    COMP-3.          
014500*                                    * 7 BLANKA RADER                     
014600   03  PRT-AFTER-9               PIC S9(3)   VALUE +9    COMP-3.          
014700*                                    * 8 BLANKA RADER                     
014930   03  PRT-EQUAL-40              PIC S9(3)   VALUE +840  COMP-3.          
014940*                                    * SKRIVER PÅ RAD 40                  
015000   03  PRT-EQUAL-42              PIC S9(3)   VALUE +842  COMP-3.          
015100*                                    * SKRIVER PÅ RAD 42                  
015200   03  PRT-EQUAL-43              PIC S9(3)   VALUE +843  COMP-3.          
015300*                                    * SKRIVER PÅ RAD 43                  
015400   03  PRT-EQUAL-46              PIC S9(3)   VALUE +846  COMP-3.          
015500*                                    * SKRIVER PÅ RAD 46                  
015600   03  PRT-EQUAL-47              PIC S9(3)   VALUE +847  COMP-3.          
015700*                                    * SKRIVER PÅ RAD 47                  
015800   03  PRT-EQUAL-48              PIC S9(3)   VALUE +848  COMP-3.          
015900*                                    * SKRIVER PÅ RAD 48                  
015901   03  PRT-EQUAL-49              PIC S9(3)   VALUE +849  COMP-3.          
015902*                                    * SKRIVER PÅ RAD 49                  
015910   03  PRT-EQUAL-51              PIC S9(3)   VALUE +851  COMP-3.          
015920*                                    * SKRIVER PÅ RAD 51                  
015930   03  PRT-EQUAL-55              PIC S9(3)   VALUE +855  COMP-3.          
015940*                                    * SKRIVER PÅ RAD 55                  
015950   03  PRT-EQUAL-59              PIC S9(3)   VALUE +859  COMP-3.          
015960*                                    * SKRIVER PÅ RAD 59                  
016000*** END COPY W006PRAR    LENGTH=886   OLD LENGTH=784                      
