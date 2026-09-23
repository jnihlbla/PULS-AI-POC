000103 ID DIVISION.                                                             
000203                                                                          
000303 PROGRAM-ID.     W3715800.                                                
000403 AUTHOR.         HÅKAN BOHLIN.                                            
000503 DATE-WRITTEN.   19/11/29.                                                
000603 DATE-COMPILED.                                                           
000703                                                                          
000803                                                                          
000903*    FUNKTION:                                                            
001003*        CREATE EXPORT DOCUMENT FROM CORE RECEIVING SCREEN.               
001104*        SYMBOLIC PARAMETERS IDUSER/IDDC/IDDISTR/IDDC-REC.                
001201*                                                                         
001301*        PROGRAM READ      WDR2 (EVENT 3147)                              
001401*        PROGRAM READ      WDM6                                           
001501*        PROGRAM READ      WDB6                                           
001601*        PROGRAM READ      WDK6                                           
001701*        PROGRAM READ      WDD3                                           
001803*        PROGRAM CALLS     W335PRIS                                       
001901*                                                                         
002001*    ABENDKODER:                                                          
002101*        U0016 -  . . . .                                                 
002201*        U1000 -  . . . .                                                 
002301*                                                                         
002403*                                                                         
002503                                                                          
002603     SKIP3                                                                
002703 ENVIRONMENT DIVISION.                                                    
002803     SKIP2                                                                
002903 INPUT-OUTPUT SECTION.                                                    
003003                                                                          
003103 FILE-CONTROL.                                                            
003203     SKIP2                                                                
003303*                                                                         
003403     SELECT INDATA                     ASSIGN TO W37158D1.                
003503     SELECT W37151                     ASSIGN TO W37158D2.                
003603     SELECT W37152                     ASSIGN TO W37158D3.                
003703     EJECT                                                                
003803 DATA DIVISION.                                                           
003903     SKIP3                                                                
004003 FILE SECTION.                                                            
004103     EJECT                                                                
004203 FD  INDATA                                                               
004303     RECORDING       F                                                    
004403     BLOCK CONTAINS  0.                                                   
004503     SKIP2                                                                
004603 01  IN-RECORD                   PIC X(80).                               
004703     EJECT                                                                
004803 FD  W37151                                                               
004903     RECORDING       V                                                    
005003     BLOCK CONTAINS  0.                                                   
005103                                                                          
005203 01  UTPOST                      PIC X(135).                              
005303     EJECT                                                                
005403 FD  W37152                                                               
005503     RECORDING       V                                                    
005603     BLOCK CONTAINS  0.                                                   
005703                                                                          
005803 01  UTPOST2                     PIC X(134).                              
005903     EJECT                                                                
006003 WORKING-STORAGE SECTION.                                                 
006103                                                                          
006203 77  IDPGM                      PIC X(8)    VALUE 'W3715800'.             
006303 77  YES                        PIC X       VALUE 'J'.                    
006403 77  NOO                        PIC X       VALUE 'N'.                    
006503 77  SAVE-IDDISTR               PIC S9(5)   VALUE ZERO COMP-3.            
006603 77  WS-IDDC-SND                PIC X(2)    VALUE SPACE.                  
006703 77  WS-VKART                   PIC S9(7)   VALUE ZERO COMP-3.            
006803 77  WS-VKART-UNIT              PIC S9(7)V9(2) VALUE ZERO COMP-3.         
006903 77  WS-VKART-TOT               PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007003 77  WS-SUVKART-TOT             PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007103                                                                          
007203 77  WS-SUFKTUTL-TOT            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007303 77  WS-PRFKTUTL-ROW            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007403 77  WS-PRFKTUTL                PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007503 77  INDX                       PIC S9(2) COMP SYNC.                      
007603 77  WS-PAGE                    PIC S9(2)      VALUE ZERO.                
007703                                                                          
007803 01  DAGENS-DATUM.                                                        
007903     03  DAGENS-AA               PIC 9(2)  VALUE ZERO.                    
008003     03  DAGENS-MM               PIC 9(2)  VALUE ZERO.                    
009003     03  DAGENS-DD               PIC 9(2)  VALUE ZERO.                    
010003                                                                          
010103 01  PRT-AREA.                                                            
010203     03 WS-RAD                   PIC X(130).                              
010303     SKIP2                                                                
010403 77  WRITE-HEADER-SW             PIC X       VALUE 'N'.                   
010503     88  WRITE-HEADER                        VALUE 'J'.                   
010603 77  PROFORMA-END-SW             PIC X       VALUE 'N'.                   
010703     88  PROFORMA-END                        VALUE 'J'.                   
010803     88  PROFORMA-LINES-LEFT                 VALUE 'N'.                   
010903 77  REPORT-SW                   PIC X       VALUE 'N'.                   
011003     88  REPORT-END                          VALUE 'J'.                   
012003 01  ERRTEXT.                                                             
013003     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
013103     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
013203     EJECT                                                                
013303 01  DYNAMISKA-SUBPROGRAM.                                                
013403*                                                                         
013503     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013603     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013703     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013803     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013903     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
014003     EJECT                                                                
014103*    --- PARAMETRAR TILL POSTSUM                                          
014203*                                                                         
014303*01  -COPY W0005   -PRE  POSTSUM-                                         
014403     EJECT                                                                
014503*    -- VALID IDDC CODES                                                  
014603*                                                                         
014703*01  -COPY WWDC99                                                         
014803*01  -COPY WWDCKONS                                                       
014903     EJECT                                                                
015003 01  FILLER                  PIC X(16) VALUE 'PRISTILL-AREA'.             
016003*01  PRIS-AREA  -COPY W335PRIS                                            
016103     SKIP2                                                                
016203 01  RAD                         PIC X(130).                              
016303     EJECT                                                                
016403 01  IN-AREA-START               PIC X(16)   VALUE                        
016503                                 'IN-AREA-START  '.                       
016603     SKIP2                                                                
016703 01  IN-AREA-1.                                                           
016803     03  IN-IDUSER               PIC X(8).                                
016903     03  FILLER                  PIC X(72).                               
017003 01  IN-AREA-2.                                                           
017103     03  IN-IDDC                 PIC X(2).                                
017203     03  FILLER                  PIC X(78).                               
017303 01  IN-AREA-3.                                                           
017403     03  IN-IDDISTR              PIC 9(5).                                
017503     03  FILLER                  PIC X(75).                               
017603 01  IN-AREA-4.                                                           
017703     03  IN-IDDC-REC             PIC X(2).                                
017803     03  FILLER                  PIC X(78).                               
017903     EJECT                                                                
018001 01  UT-AREA-START              PIC X(24)   VALUE                         
018101                                'UT-AREA-START  '.                        
018201     SKIP2                                                                
018301 01  UT-RAD                     PIC X(130)  VALUE SPACE.                  
018401     EJECT                                                                
018501                                                                          
018601*    --- LISTAN                                                           
018701 01  RUBRIK-RAD.                                                          
018801     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
018901     03  FILLER            PIC X(1)  VALUE SPACE.                         
019001     03  FILLER            PIC X(8)  VALUE 'EXCHANGE'.                    
019101     03  FILLER            PIC X(1)  VALUE SPACE.                         
019201     03  FILLER            PIC X(15) VALUE 'EXPORT PROFORMA'.             
019301     03  FILLER            PIC X(6)  VALUE SPACE.                         
019401     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
019501     03  LIST-AAR          PIC X(2).                                      
019601     03  FILLER            PIC X(1) VALUE '/'.                            
019701     03  LIST-MAN          PIC X(2).                                      
019801     03  FILLER            PIC X(1) VALUE '/'.                            
019901     03  LIST-DAG          PIC X(2).                                      
020001     03  FILLER            PIC X(13) VALUE SPACE.                         
020102     03  FILLER            PIC X(15) VALUE 'W37158-001'.                  
020201* ANTAL BYTES                                                             
020301                                                                          
020401 01  E-RUBRIK-RAD.                                                        
020501     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
020601     03  FILLER            PIC X(1)  VALUE ';'.                           
020701     03  FILLER            PIC X(8)  VALUE 'EXCHANGE'.                    
020801     03  FILLER            PIC X(1)  VALUE SPACE.                         
020901     03  FILLER            PIC X(15) VALUE 'EXPORT PROFORMA'.             
021001     03  FILLER            PIC X(1)  VALUE ';'.                           
021101     03  FILLER            PIC X(1)  VALUE ';'.                           
021201     03  FILLER            PIC X(1)  VALUE ';'.                           
021301     03  FILLER            PIC X(1)  VALUE ';'.                           
021401     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
021501     03  FILLER            PIC X(1)  VALUE ';'.                           
021601     03  E-LIST-AAR        PIC X(2).                                      
021701     03  FILLER            PIC X(1) VALUE '/'.                            
021801     03  E-LIST-MAN        PIC X(2).                                      
021901     03  FILLER            PIC X(1) VALUE '/'.                            
022001     03  E-LIST-DAG        PIC X(2).                                      
022101     03  FILLER            PIC X(1)  VALUE ';'.                           
022202     03  FILLER            PIC X(15) VALUE 'W37158-001'.                  
022301     03  FILLER            PIC X(1)  VALUE ';'.                           
022401     03  FILLER            PIC X(1)  VALUE ';'.                           
022502     03  FILLER            PIC X(1)  VALUE ';'.                           
022601* ANTAL BYTES                                                             
022701                                                                          
022801 01  U-RUBRIK-RAD1.                                                       
022901     03  FILLER            PIC X(10) VALUE 'SHIPPER   '.                  
023001     03  FILLER            PIC X(34) VALUE SPACE.                         
023101     03  FILLER            PIC X(9) VALUE 'CONSIGNEE'.                    
023201     03  FILLER            PIC X(12) VALUE SPACE.                         
023301     03  FILLER            PIC X(5) VALUE 'PAGE'.                         
023401     03  LIST-SIDA         PIC Z(1)9 VALUE ZERO.                          
023501* ANTAL BYTES                                                             
023601                                                                          
023701 01  E-U-RUBRIK-RAD1.                                                     
023801     03  FILLER            PIC X(10) VALUE 'SHIPPER   '.                  
023901     03  FILLER            PIC X(1)  VALUE ';'.                           
024001     03  FILLER            PIC X(1)  VALUE ';'.                           
024101     03  FILLER            PIC X(1)  VALUE ';'.                           
024201     03  FILLER            PIC X(1)  VALUE ';'.                           
024301     03  FILLER            PIC X(9)  VALUE 'CONSIGNEE'.                   
024401     03  FILLER            PIC X(1)  VALUE ';'.                           
024501     03  FILLER            PIC X(1)  VALUE ';'.                           
024601     03  FILLER            PIC X(1)  VALUE ';'.                           
024701     03  FILLER            PIC X(1)  VALUE ';'.                           
024801     03  FILLER            PIC X(1)  VALUE ';'.                           
024902     03  FILLER            PIC X(1)  VALUE ';'.                           
025001* ANTAL BYTES                                                             
025101                                                                          
025201 01  U-RUBRIK-RAD2.                                                       
025301     03  LIST-BEAVS-RAD1 PIC X(35)    VALUE SPACE.                        
025401     03  FILLER          PIC X(9)     VALUE SPACE.                        
025501     03  LIST-BEGMT-RAD1 PIC X(35)    VALUE SPACE.                        
025601* ANTAL BYTES                 116                                         
025701                                                                          
025801 01  E-U-RUBRIK-RAD2.                                                     
025901     03  E-LIST-BEAVS-RAD1 PIC X(35) VALUE SPACE.                         
026001     03  FILLER            PIC X(1)  VALUE ';'.                           
026101     03  FILLER            PIC X(1)  VALUE ';'.                           
026201     03  FILLER            PIC X(1)  VALUE ';'.                           
026301     03  FILLER            PIC X(1)  VALUE ';'.                           
026401     03  E-LIST-BEGMT-RAD1 PIC X(35) VALUE SPACE.                         
026501     03  FILLER            PIC X(1)  VALUE ';'.                           
026601     03  FILLER            PIC X(1)  VALUE ';'.                           
026701     03  FILLER            PIC X(1)  VALUE ';'.                           
026801     03  FILLER            PIC X(1)  VALUE ';'.                           
026901     03  FILLER            PIC X(1)  VALUE ';'.                           
027002     03  FILLER            PIC X(1)  VALUE ';'.                           
027101* ANTAL BYTES                 116                                         
027201                                                                          
027301 01  U-RUBRIK-RAD3.                                                       
027401     03  LIST-BEAVS-RAD2   PIC X(35)  VALUE SPACE.                        
027501     03  FILLER            PIC X(9)   VALUE SPACE.                        
027601     03  LIST-BEGMT-RAD2   PIC X(35)  VALUE SPACE.                        
027701                                                                          
027801 01  E-U-RUBRIK-RAD3.                                                     
027901     03  E-LIST-BEAVS-RAD2   PIC X(35) VALUE SPACE.                       
028001     03  FILLER              PIC X(1)  VALUE ';'.                         
028101     03  FILLER              PIC X(1)  VALUE ';'.                         
028201     03  FILLER              PIC X(1)  VALUE ';'.                         
028301     03  FILLER              PIC X(1)  VALUE ';'.                         
028401     03  E-LIST-BEGMT-RAD2   PIC X(35) VALUE SPACE.                       
028501     03  FILLER              PIC X(1)  VALUE ';'.                         
028601     03  FILLER              PIC X(1)  VALUE ';'.                         
028701     03  FILLER              PIC X(1)  VALUE ';'.                         
028801     03  FILLER              PIC X(1)  VALUE ';'.                         
028901     03  FILLER              PIC X(1)  VALUE ';'.                         
029002     03  FILLER              PIC X(1)  VALUE ';'.                         
029101                                                                          
029201 01  U-RUBRIK-RAD4.                                                       
029301     03  LIST-ADAVS-GATA   PIC X(35)  VALUE SPACE.                        
029401     03  FILLER            PIC X(9)   VALUE SPACE.                        
029501     03  LIST-ADGMT-GATA   PIC X(35)  VALUE SPACE.                        
029601     03  FILLER            PIC X(1)   VALUE SPACE.                        
029701                                                                          
029801 01  E-U-RUBRIK-RAD4.                                                     
029901     03  E-LIST-ADAVS-GATA   PIC X(35) VALUE SPACE.                       
030001     03  FILLER              PIC X(1)  VALUE ';'.                         
030101     03  FILLER              PIC X(1)  VALUE ';'.                         
030201     03  FILLER              PIC X(1)  VALUE ';'.                         
030301     03  FILLER              PIC X(1)  VALUE ';'.                         
030401     03  E-LIST-ADGMT-GATA   PIC X(35) VALUE SPACE.                       
030501     03  FILLER              PIC X(1)  VALUE ';'.                         
030601     03  FILLER              PIC X(1)  VALUE ';'.                         
030701     03  FILLER              PIC X(1)  VALUE ';'.                         
030801     03  FILLER              PIC X(1)  VALUE ';'.                         
030901     03  FILLER              PIC X(1)  VALUE ';'.                         
031002     03  FILLER              PIC X(1)  VALUE ';'.                         
031101                                                                          
031201 01  U-RUBRIK-RAD5.                                                       
031301     03  LIST-ADAVS-PADR   PIC X(35) VALUE SPACE.                         
031401     03  FILLER            PIC X(9)  VALUE SPACE.                         
031501     03  LIST-ADGMT-PADR   PIC X(35) VALUE SPACE.                         
031601     03  FILLER            PIC X(36) VALUE SPACE.                         
031701                                                                          
031801 01  E-U-RUBRIK-RAD5.                                                     
031901     03  E-LIST-ADAVS-PADR   PIC X(35) VALUE SPACE.                       
032001     03  FILLER              PIC X(1)  VALUE ';'.                         
032101     03  FILLER              PIC X(1)  VALUE ';'.                         
032201     03  FILLER              PIC X(1)  VALUE ';'.                         
032301     03  FILLER              PIC X(1)  VALUE ';'.                         
032401     03  E-LIST-ADGMT-PADR   PIC X(35) VALUE SPACE.                       
032501     03  FILLER              PIC X(1)  VALUE ';'.                         
032601     03  FILLER              PIC X(1)  VALUE ';'.                         
032701     03  FILLER              PIC X(1)  VALUE ';'.                         
032801     03  FILLER              PIC X(1)  VALUE ';'.                         
032901     03  FILLER              PIC X(1)  VALUE ';'.                         
033002     03  FILLER              PIC X(1)  VALUE ';'.                         
033101                                                                          
033201 01  U-RUBRIK-RAD6.                                                       
033301     03  LIST-ADAVS-LAND   PIC X(35) VALUE SPACE.                         
033401     03  FILLER            PIC X(9)  VALUE SPACE.                         
033501     03  LIST-ADGMT-LAND   PIC X(35) VALUE SPACE.                         
033601     03  FILLER            PIC X(36) VALUE SPACE.                         
033701                                                                          
033801 01  E-U-RUBRIK-RAD6.                                                     
033901     03  E-LIST-ADAVS-LAND   PIC X(35) VALUE SPACE.                       
034001     03  FILLER              PIC X(1)  VALUE ';'.                         
034101     03  FILLER              PIC X(1)  VALUE ';'.                         
034201     03  FILLER              PIC X(1)  VALUE ';'.                         
034301     03  FILLER              PIC X(1)  VALUE ';'.                         
034401     03  E-LIST-ADGMT-LAND   PIC X(35) VALUE SPACE.                       
034501     03  FILLER              PIC X(1)  VALUE ';'.                         
034601     03  FILLER              PIC X(1)  VALUE ';'.                         
034701     03  FILLER              PIC X(1)  VALUE ';'.                         
034801     03  FILLER              PIC X(1)  VALUE ';'.                         
034901     03  FILLER              PIC X(1)  VALUE ';'.                         
035002     03  FILLER              PIC X(1)  VALUE ';'.                         
035101                                                                          
035201 01  U-RUBRIK-RAD6A.                                                      
035301     03  FILLER            PIC X(11) VALUE 'DC NUMBER:'.                  
035401     03  LIST-IDDC-SND     PIC X(2)  VALUE SPACE.                         
035501     03  FILLER            PIC X(3)  VALUE SPACE.                         
035601     03  FILLER            PIC X(8)  VALUE 'DISTR.NO'.                    
035701     03  FILLER            PIC X(1)  VALUE SPACE.                         
035801     03  LIST-IDDISTR-SND  PIC Z(4)9 VALUE ZERO.                          
035901     03  FILLER            PIC X(14) VALUE SPACE.                         
036001     03  FILLER            PIC X(10) VALUE 'DISTR.NO. '.                  
036104     03  LIST-IDDISTR      PIC Z(5)  VALUE ZERO.                          
036201                                                                          
036301 01  E-U-RUBRIK-RAD6A.                                                    
036401     03  FILLER              PIC X(11) VALUE 'DC NUMBER:'.                
036501     03  E-LIST-IDDC-SND     PIC X(2)  VALUE SPACE.                       
036601     03  FILLER              PIC X(1)  VALUE ';'.                         
036701     03  FILLER              PIC X(8)  VALUE 'DISTR.NO'.                  
036801     03  FILLER              PIC X(1)  VALUE SPACE.                       
036901     03  E-LIST-IDDISTR-SND  PIC Z(3)9 VALUE ZERO.                        
037001     03  FILLER              PIC X(1)  VALUE ';'.                         
037102     03  FILLER              PIC X(1)  VALUE ';'.                         
037201     03  FILLER              PIC X(1)  VALUE ';'.                         
037301     03  FILLER              PIC X(10) VALUE 'DISTR.NO. '.                
037404     03  E-LIST-IDDISTR      PIC Z(5)  VALUE ZERO.                        
037501     03  FILLER              PIC X(1)  VALUE ';'.                         
037601     03  FILLER              PIC X(1)  VALUE ';'.                         
037701     03  FILLER              PIC X(1)  VALUE ';'.                         
037801     03  FILLER              PIC X(1)  VALUE ';'.                         
037901     03  FILLER              PIC X(1)  VALUE ';'.                         
038002     03  FILLER              PIC X(1)  VALUE ';'.                         
038101                                                                          
038201 01  U-RUBRIK-LINJE.                                                      
038301     03  FILLER            PIC X(92) VALUE ALL '_'.                       
038401                                                                          
038501 01  E-U-RUBRIK-LINJE.                                                    
038601     03  FILLER            PIC X(10) VALUE ALL ';;;;;;;;;;'.              
038701                                                                          
038801 01  U-RUBRIK-RAD6C.                                                      
038901     03  FILLER            PIC X(3) VALUE SPACE.                          
039001     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
039101     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
039201     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
039301     03  FILLER            PIC X(8) VALUE 'VALUE **'.                     
039401                                                                          
039501 01  E-U-RUBRIK-RAD6C.                                                    
039601     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
039701     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
039801     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
039901     03  FILLER            PIC X(8) VALUE 'VALUE * '.                     
040001     03  FILLER            PIC X(10) VALUE ';;;;;;;;;;'.                  
040101                                                                          
040201 01  SISTA-RADEN1.                                                        
040301     03  FILLER            PIC X(14)      VALUE 'NET WEIGHT '.            
040401     03  LIST-SUVKART-TOT  PIC Z(7)9.9(2) VALUE ZERO.                     
040501     03  FILLER            PIC X(1)       VALUE SPACE.                    
040601     03  LIST-UOM-TWEIGHT  PIC X(3)       VALUE SPACE.                    
040701                                                                          
040801 01  E-SISTA-RADEN1.                                                      
040901     03  FILLER              PIC X(14)    VALUE 'NET WEIGHT '.            
041001     03  E-LIST-SUVKART-TOT  PIC Z(7)9.9(2) VALUE ZERO.                   
041101     03  E-LIST-UOM-TWEIGHT  PIC X(4)       VALUE SPACE.                  
041201     03  FILLER              PIC X(1)       VALUE ';'.                    
041301     03  FILLER              PIC X(1)       VALUE ';'.                    
041401     03  FILLER              PIC X(1)       VALUE ';'.                    
041501     03  FILLER              PIC X(1)       VALUE ';'.                    
041601     03  FILLER              PIC X(1)       VALUE ';'.                    
041701     03  FILLER              PIC X(1)       VALUE ';'.                    
041801     03  FILLER              PIC X(1)       VALUE ';'.                    
041901     03  FILLER              PIC X(1)       VALUE ';'.                    
042002     03  FILLER              PIC X(1)       VALUE ';'.                    
042101                                                                          
042201 01  SISTA-RADEN2.                                                        
042301     03  FILLER            PIC X(17)     VALUE 'GROSS WEIGHT '.           
042401     03  LIST-GROSS-WEIGHT PIC Z(7)9     VALUE ZERO.                      
042501     03  FILLER            PIC X(1)      VALUE SPACE.                     
042601     03  LIST-UOM-WEIGHT   PIC X(3)      VALUE SPACE.                     
042701                                                                          
042801 01  E-SISTA-RADEN2.                                                      
042901     03  FILLER              PIC X(17)     VALUE 'GROSS WEIGHT '.         
043001     03  E-LIST-GROSS-WEIGHT PIC Z(7)9     VALUE ZERO.                    
043101     03  E-LIST-UOM-WEIGHT   PIC X(4)      VALUE SPACE.                   
043201     03  FILLER              PIC X(1)      VALUE ';'.                     
043301     03  FILLER              PIC X(1)      VALUE ';'.                     
043401     03  FILLER              PIC X(1)      VALUE ';'.                     
043501     03  FILLER              PIC X(1)      VALUE ';'.                     
043601     03  FILLER              PIC X(1)      VALUE ';'.                     
043701     03  FILLER              PIC X(1)      VALUE ';'.                     
043801     03  FILLER              PIC X(1)      VALUE ';'.                     
043901     03  FILLER              PIC X(1)      VALUE ';'.                     
044002     03  FILLER              PIC X(1)      VALUE ';'.                     
044101                                                                          
044201 01  SISTA-RADEN3.                                                        
044301     03  FILLER             PIC X(16)     VALUE 'TOTAL VOLUME '.          
044401     03  LIST-VLORDBTO-TOT  PIC Z(6).9(2) VALUE ZERO.                     
044501     03  FILLER             PIC X(1)      VALUE SPACE.                    
044601     03  LIST-UOM-VOLUME    PIC X(9)      VALUE SPACE.                    
044701                                                                          
044801 01  E-SISTA-RADEN3.                                                      
044901     03  FILLER               PIC X(17)     VALUE 'TOTAL VOLUME '.        
045001     03  E-LIST-VLORDBTO-TOT  PIC Z(6).9(2) VALUE ZERO.                   
045101     03  E-LIST-UOM-VOLUME    PIC X(10)     VALUE SPACE.                  
045201     03  FILLER               PIC X(1)      VALUE ';'.                    
045301     03  FILLER               PIC X(1)      VALUE ';'.                    
045401     03  FILLER               PIC X(1)      VALUE ';'.                    
045501     03  FILLER               PIC X(1)      VALUE ';'.                    
045601     03  FILLER               PIC X(1)      VALUE ';'.                    
045701     03  FILLER               PIC X(1)      VALUE ';'.                    
045801     03  FILLER               PIC X(1)      VALUE ';'.                    
045901     03  FILLER               PIC X(1)      VALUE ';'.                    
046002     03  FILLER               PIC X(1)      VALUE ';'.                    
046101                                                                          
046201 01  SISTA-RADEN4.                                                        
046301     03  FILLER            PIC X(15)     VALUE 'TOTAL VALUE '.            
046401     03  LIST-SUFKTUTL-TOT PIC Z(7).9(2) VALUE ZERO.                      
046501     03  FILLER            PIC X(1)  VALUE SPACE.                         
046601     03  LIST-KDVALISO     PIC X(3).                                      
046701     03  FILLER            PIC X(103) VALUE SPACE.                        
046801                                                                          
046901 01  E-SISTA-RADEN4.                                                      
047001     03  FILLER              PIC X(12)     VALUE 'TOTAL VALUE '.          
047101     03  E-LIST-SUFKTUTL-TOT PIC Z(7).9(2) VALUE ZERO.                    
047201     03  FILLER              PIC X(1)      VALUE SPACE.                   
047301     03  E-LIST-KDVALISO     PIC X(3).                                    
047401     03  FILLER              PIC X(1)      VALUE ';'.                     
047501     03  FILLER              PIC X(1)      VALUE ';'.                     
047601     03  FILLER              PIC X(1)      VALUE ';'.                     
047701     03  FILLER              PIC X(1)      VALUE ';'.                     
047801     03  FILLER              PIC X(1)      VALUE ';'.                     
047901     03  FILLER              PIC X(1)      VALUE ';'.                     
048001     03  FILLER              PIC X(1)      VALUE ';'.                     
048102     03  FILLER              PIC X(1)      VALUE ';'.                     
048203     03  FILLER              PIC X(1)      VALUE ';'.                     
048301                                                                          
048401 01  SISTA-RADEN5.                                                        
048501     03  FILLER            PIC X(8)  VALUE '** NO CO'.                    
048601     03  FILLER            PIC X(19) VALUE 'MMERCIAL VALUE STAT'.         
048701     03  FILLER            PIC X(19) VALUE 'ED FOR CUSTOMS PURP'.         
048801     03  FILLER            PIC X(12) VALUE 'OSES ONLY **'.                
048901     03  FILLER            PIC X(70) VALUE SPACE.                         
049001                                                                          
049101 01  E-SISTA-RADEN5.                                                      
049201     03  FILLER            PIC X(8)  VALUE '** NO CO'.                    
049301     03  FILLER            PIC X(19) VALUE 'MMERCIAL VALUE STAT'.         
049401     03  FILLER            PIC X(19) VALUE 'ED FOR CUSTOMS PURP'.         
049501     03  FILLER            PIC X(12) VALUE 'OSES ONLY * '.                
049601     03  FILLER            PIC X(9)  VALUE ';;;;;;;;;'.                   
049701                                                                          
049801 01  SISTA-RADEN6.                                                        
049901     03  FILLER            PIC X(8)  VALUE 'SHIPPERS'.                    
050001     03  FILLER            PIC X(15) VALUE ' SIGNATURE AND '.             
050101     03  FILLER            PIC X(19) VALUE 'TITLE ____________ '.         
050201     03  FILLER            PIC X(70) VALUE ALL '_'.                       
050301                                                                          
050401 01  E-SISTA-RADEN6.                                                      
050501     03  FILLER            PIC X(8)  VALUE 'SHIPPERS'.                    
050601     03  FILLER            PIC X(15) VALUE ' SIGNATURE AND '.             
050701     03  FILLER            PIC X(19) VALUE 'TITLE ____________ '.         
050801     03  FILLER            PIC X(20) VALUE '____________________'.        
050901     03  FILLER            PIC X(1)  VALUE ';'.                           
051001     03  FILLER            PIC X(1)  VALUE ';'.                           
051101     03  FILLER            PIC X(1)  VALUE ';'.                           
051201     03  FILLER            PIC X(1)  VALUE ';'.                           
051301     03  FILLER            PIC X(1)  VALUE ';'.                           
051401     03  FILLER            PIC X(1)  VALUE ';'.                           
051501     03  FILLER            PIC X(1)  VALUE ';'.                           
051601     03  FILLER            PIC X(1)  VALUE ';'.                           
051702     03  FILLER            PIC X(1)  VALUE ';'.                           
051801                                                                          
051901                                                                          
052001 01  U-RUBRIK-RAD7.                                                       
052101     03  FILLER            PIC X(9) VALUE '  PART NO'.                    
052201     03  FILLER            PIC X(3) VALUE SPACE.                          
052301     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
052401     03  FILLER            PIC X(10) VALUE SPACE.                         
052501     03  FILLER            PIC X(1) VALUE SPACE.                          
052602     03  FILLER            PIC X(10) VALUE 'REPORT NO ' .                 
052703     03  FILLER            PIC X(2) VALUE SPACE.                          
052801     03  FILLER            PIC X(5) VALUE 'Q.DEL' .                       
052901     03  FILLER            PIC X(4) VALUE SPACE.                          
053001     03  FILLER            PIC X(7) VALUE 'T-PRICE' .                     
053101     03  FILLER            PIC X(3) VALUE SPACE.                          
053201     03  FILLER            PIC X(3) VALUE 'ORG'.                          
053301     03  FILLER            PIC X(6) VALUE SPACE.                          
053401     03  FILLER            PIC X(7) VALUE 'STAT.NO'.                      
053501     03  FILLER            PIC X(3) VALUE SPACE.                          
053601     03  FILLER            PIC X(8) VALUE 'T-WEIGHT'.                     
053701                                                                          
053801 01  E-U-RUBRIK-RAD7.                                                     
053901     03  FILLER            PIC X(9)  VALUE '  PART NO'.                   
054001     03  FILLER            PIC X(1)  VALUE ';'.                           
054101     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
054201     03  FILLER            PIC X(1)  VALUE ';'.                           
054302     03  FILLER            PIC X(10) VALUE 'REPORT NO ' .                 
054403     03  FILLER            PIC X(1)  VALUE ';'.                           
054501     03  FILLER            PIC X(5)  VALUE 'Q.DEL' .                      
054601     03  FILLER            PIC X(1)  VALUE ';'.                           
054701     03  FILLER            PIC X(7)  VALUE 'T-PRICE' .                    
054801     03  FILLER            PIC X(1)  VALUE ';'.                           
054901     03  FILLER            PIC X(3)  VALUE 'ORG'.                         
055001     03  FILLER            PIC X(1)  VALUE ';'.                           
055101     03  FILLER            PIC X(7)  VALUE 'STAT.NO'.                     
055201     03  FILLER            PIC X(1)  VALUE ';'.                           
055301     03  FILLER            PIC X(8)  VALUE 'T-WEIGHT'.                    
055401     03  FILLER            PIC X(1)  VALUE ';'.                           
055502     03  FILLER            PIC X(1)  VALUE ';'.                           
055603     03  FILLER            PIC X(1)  VALUE ';'.                           
055701                                                                          
055801 01  U-RAD.                                                               
055901     03  LIST-IDARTNR      PIC Z(9).                                      
056001     03  FILLER            PIC X(3) VALUE SPACE.                          
056101     03  LIST-BEART        PIC X(20).                                     
056201     03  FILLER            PIC X(3) VALUE SPACE.                          
056301     03  LIST-IDBYTRAP     PIC Z(6)9.                                     
056402     03  FILLER            PIC X(4) VALUE SPACE.                          
056503     03  LIST-KVLEVART     PIC Z(4).                                      
056601     03  FILLER            PIC X(2) VALUE SPACE.                          
056701     03  LIST-PRFKTUTL-RAD PIC Z(6).9(2) VALUE ZERO.                      
056801     03  FILLER            PIC X(3) VALUE SPACE.                          
056901     03  LIST-KDARTURS     PIC X(3) VALUE SPACE.                          
057001     03  FILLER            PIC X(4) VALUE SPACE.                          
057101     03  LIST-IDSTATNR     PIC Z(8)9 VALUE ZERO.                          
057201     03  FILLER            PIC X(1) VALUE SPACE.                          
057301     03  LIST-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                     
057401                                                                          
057501 01  E-U-RAD.                                                             
057601     03  E-LIST-IDARTNR      PIC Z(9).                                    
057701     03  FILLER              PIC X(1)  VALUE ';'.                         
057801     03  E-LIST-BEART        PIC X(20).                                   
057902     03  FILLER              PIC X(1)  VALUE ';'.                         
058003     03  E-LIST-IDBYTRAP     PIC Z(6)9 VALUE ZERO.                        
058101     03  FILLER              PIC X(1)  VALUE ';'.                         
058201     03  E-LIST-KVLEVART     PIC Z(4).                                    
058301     03  FILLER              PIC X(1)  VALUE ';'.                         
058401     03  E-LIST-PRFKTUTL-RAD PIC Z(6).9(2) VALUE ZERO.                    
058503     03  FILLER              PIC X(1)  VALUE ';'.                         
058603     03  E-LIST-KDARTURS     PIC X(3) VALUE SPACE.                        
058702     03  FILLER              PIC X(1)  VALUE ';'.                         
058803     03  E-LIST-IDSTATNR     PIC Z(8)9 VALUE ZERO.                        
058903     03  FILLER              PIC X(1)  VALUE ';'.                         
059003     03  E-LIST-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                   
059103     03  FILLER              PIC X(1)  VALUE ';'.                         
059202     03  FILLER              PIC X(1)  VALUE ';'.                         
059303     03  FILLER              PIC X(1)  VALUE ';'.                         
059403     EJECT                                                                
059503 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
059603     SKIP3                                                                
059703                                                                          
059803 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
059903 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
060003 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
061003 01  WS-IMS-SEKTION          PIC X(30)   VALUE SPACE.                     
062003                                                                          
062103 01  NYCKLAR-TILL-DLI.                                                    
062203     03  W-WDB601-X.                                                      
062303         05  W-IDDC          PIC X(2)    VALUE SPACE.                     
062403     03  W-KDSEGKEY-X.                                                    
062503         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
062603     03  W-IDARTNR-X.                                                     
062703         05  W-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.               
062803     03  W-IDDISTR-X.                                                     
062903         05  W-IDDISTR-2     PIC S9(5)   VALUE ZERO COMP-3.               
063003     03  W-IDSKYLT-X.                                                     
064003         05  W-IDSKYLT       PIC X(3)    VALUE 'GB'.                      
064103     03  W-WDM601KY-X.                                                    
064203         05  W-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.               
064303         05  W-IDBYTRAP      PIC S9(7)   VALUE ZERO COMP-3.               
064403     03  W-WDGXKEY-3147-X.                                                
064503         05  W-3147-IDHTYP   PIC X(4)    VALUE '3147'.                    
064603         05  FILLER          PIC X(26)   VALUE LOW-VALUE.                 
064703     03  W-3148KY-X.                                                      
064803         05  W-3148-IDUSER   PIC X(8)    VALUE SPACE.                     
064903         05  W-3148-IDDC     PIC X(2)    VALUE SPACE.                     
064904         05  W-3148-IDDC-REC PIC X(2)    VALUE SPACE.                     
065003     03  W-3150KY-X.                                                      
065103         05  W-3150-IDDISTR  PIC S9(5)   VALUE ZERO COMP-3.               
065203         05  W-3150-IDBYTRAP PIC S9(7)   VALUE ZERO COMP-3.               
065303     SKIP2                                                                
065403*    --- STATUS-KOD FRÅN IMS                                              
065503 01  STATUS-WS                   PIC XX.                                  
065603     88  SEGMENT-FINNS                       VALUE '  '.                  
065703     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
065803     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
065903     88  SEGMENT-SLUT                        VALUE 'GB'.                  
066003     88  IMS-EJ-OK                           VALUE 'XD'.                  
066103     SKIP2                                                                
066203 01  GODK-STATUSKODER.                                                    
066303     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
066403     SKIP3                                                                
066503 01  SSA1                        PIC X(80).                               
066603 01  SSA2                        PIC X(80).                               
066703     EJECT                                                                
066803*    --- IMS FUNKTIONSKODER                                               
066903*01  -COPY W0003                                                          
067003     EJECT                                                                
067103*    ---  DLI INPUT-OUTPUT AREA                                           
067203                                                                          
067303 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
067403 01  DLI-IO-WDB601.                                                       
067503*    03  -COPY WDB601  -PRE WDB6-                                         
067603     EJECT                                                                
067703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
067803 01  DLI-IO-WDK611.                                                       
067903*    03  -COPY WDK611  -PRE WDK611-                                       
068003     EJECT                                                                
069003 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
070003 01  DLI-IO-WDD311.                                                       
071003*    03  -COPY WDD311  -PRE WDD311-                                       
071103     EJECT                                                                
071203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM601'.                      
071303 01  DLI-IO-WDM601.                                                       
071403*    03  -COPY WDM601  -PRE WDM6-                                         
071503     EJECT                                                                
071603 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM611'.                      
071703 01  DLI-IO-WDM611.                                                       
071803*    03  -COPY WDM611  -PRE WDM6-                                         
071901     EJECT                                                                
072002 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX3148'.                    
072103 01  DLI-IO-WDGX3148.                                                     
072203*    03  -COPY WDGX3148                                                   
072303     EJECT                                                                
072403 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX3150'.                    
072503 01  DLI-IO-WDGX3150.                                                     
072603*    03  -COPY WDGX3150                                                   
072701     EJECT                                                                
072801 LINKAGE SECTION.                                                         
072901                                                                          
073001*01  -COPY W0009  -PRE MSG-                                               
073103     EJECT                                                                
073203                                                                          
073303*01  -COPY W0008  -PRE 3147-                                              
073403     05  FILLER                  PIC X.                                   
073502     EJECT                                                                
073603                                                                          
073703*01  -COPY W0008  -PRE WDM6-                                              
073803     05  FILLER                  PIC X.                                   
073903     EJECT                                                                
074003                                                                          
074103*01  -COPY W0008  -PRE WDB6-                                              
074203     05  FILLER                  PIC X.                                   
074303     EJECT                                                                
074403                                                                          
074501*01  -COPY W0008  -PRE WDK6-                                              
074601     05  FILLER                  PIC X.                                   
074702     EJECT                                                                
074803                                                                          
074901*01  -COPY W0008  -PRE WDD3-                                              
075001     05  FILLER                  PIC X.                                   
075102     EJECT                                                                
075201                                                                          
075301*01  -COPY W0008 -PRE  PRIS-ARTC-.                                        
075401     05  FILLER        PIC X.                                             
075501     EJECT                                                                
075601                                                                          
075701*01  -COPY W0008 -PRE  PRIS-WDK7-.                                        
075801     05  FILLER        PIC X.                                             
075901     EJECT                                                                
076001                                                                          
076101*01  -COPY W0008 -PRE  PRIS-GMTA-.                                        
076201     05  FILLER        PIC X.                                             
076301     EJECT                                                                
076401                                                                          
076501*01  -COPY W0008 -PRE  PRIS-BETA-.                                        
076601     05  FILLER        PIC X.                                             
076701                                                                          
076801*01  -COPY W0008 -PRE  PRIS-GPRIA-.                                       
076901     05  FILLER        PIC X.                                             
077001     EJECT                                                                
077101*01  -COPY W0008 -PRE  PRIS-GPRIB-.                                       
077201     05  FILLER        PIC X.                                             
077303     EJECT                                                                
077403 01  PRIS-COST-WDK6-PCB          PIC X.                                   
077503 01  PRIS-COST-WDK7-PCB          PIC X.                                   
077603 01  PRIS-COST-WDF1-PCB          PIC X.                                   
077703 01  PRIS-COST-9305-PCB          PIC X.                                   
077803 01  PRIS-COST-WDK72-PCB         PIC X.                                   
077903 01  PRIS-COST-WDB6-PCB          PIC X.                                   
078003     EJECT                                                                
078103 PROCEDURE DIVISION  USING MSG-PCB 3147-PCB WDM6-PCB                      
078203     WDB6-PCB WDK6-PCB WDD3-PCB                                           
078303     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
078403     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
078503     PRIS-COST-WDK6-PCB                                                   
078603     PRIS-COST-WDK7-PCB                                                   
078703     PRIS-COST-WDF1-PCB                                                   
078803     PRIS-COST-9305-PCB                                                   
078903     PRIS-COST-WDK72-PCB                                                  
079003     PRIS-COST-WDB6-PCB.                                                  
079103 MAIN SECTION.                                                            
079203     ENTRY 'DLITCBL' USING MSG-PCB 3147-PCB WDM6-PCB                      
079303     WDB6-PCB WDK6-PCB WDD3-PCB                                           
079403     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
079503     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
079603     PRIS-COST-WDK6-PCB                                                   
079703     PRIS-COST-WDK7-PCB                                                   
079803     PRIS-COST-WDF1-PCB                                                   
079903     PRIS-COST-9305-PCB                                                   
080003     PRIS-COST-WDK72-PCB                                                  
080103     PRIS-COST-WDB6-PCB.                                                  
080203                                                                          
080303     PERFORM A-INIT                                                       
080403     PERFORM B-INIT-HEADER-FOOTER                                         
080503     MOVE YES TO WRITE-HEADER-SW                                          
080603     PERFORM IMS-GNP-WDGX3150                                             
080703     PERFORM C-PROFORMA-HEADER                                            
080803     IF SEGMENT-SAKNAS                                                    
080903        MOVE U-RUBRIK-LINJE TO WS-RAD                                     
081003        PERFORM S02-WRITE-UT2-RAD                                         
082003        MOVE SPACE TO WS-RAD                                              
082103        PERFORM F-CREATE-LASTPAGE                                         
082203     ELSE                                                                 
082303        MOVE 3150-IDDISTR  TO W-IDDISTR                                   
082403        MOVE 3150-IDBYTRAP TO W-IDBYTRAP                                  
082503        PERFORM IMS-GU-WDM601                                             
082603        PERFORM UNTIL PROFORMA-END                                        
082703                                                                          
082803           MOVE +1 TO INDX                                                
082903           PERFORM UNTIL INDX > 30 OR PROFORMA-END                        
083003              PERFORM D-HANDLE-ROW                                        
083103              IF REPORT-END                                               
083203                PERFORM IMS-GNP-WDGX3150                                  
083303                IF SEGMENT-FINNS                                          
083403                  MOVE 3150-IDDISTR  TO W-IDDISTR                         
083503                  MOVE 3150-IDBYTRAP TO W-IDBYTRAP                        
083603                  PERFORM IMS-GU-WDM601                                   
083703                ELSE                                                      
083803                  MOVE YES TO PROFORMA-END-SW                             
083903                END-IF                                                    
084003                MOVE NOO TO REPORT-SW                                     
084103              ELSE                                                        
084201                ADD +1 TO INDX                                            
084302              END-IF                                                      
084401           END-PERFORM                                                    
084501                                                                          
084601           MOVE U-RUBRIK-LINJE TO WS-RAD                                  
084701           PERFORM S02-WRITE-UT2-RAD                                      
084801           MOVE SPACE TO WS-RAD                                           
084901                                                                          
085001           IF INDX > 30 AND PROFORMA-LINES-LEFT                           
085101              PERFORM C-PROFORMA-HEADER                                   
085201           ELSE                                                           
085301              PERFORM F-CREATE-LASTPAGE                                   
085401              MOVE YES TO PROFORMA-END-SW                                 
085501           END-IF                                                         
085601                                                                          
085701        END-PERFORM                                                       
085801                                                                          
085901     END-IF                                                               
086001                                                                          
086101                                                                          
086201     PERFORM Z-FINIT                                                      
086301                                                                          
086401     MOVE ZERO TO RETURN-CODE                                             
086501     GOBACK                                                               
086601     .                                                                    
086701     EJECT                                                                
086801 A-INIT SECTION.                                                          
086901                                                                          
087003     OPEN INPUT  INDATA                                                   
087102     OPEN OUTPUT W37151                                                   
087201                 W37152                                                   
087301                                                                          
087402     READ INDATA INTO IN-AREA-1                                           
087503     READ INDATA INTO IN-AREA-2                                           
087603     READ INDATA INTO IN-AREA-3                                           
087703     READ INDATA INTO IN-AREA-4                                           
087803                                                                          
087901     ACCEPT DAGENS-DATUM FROM DATE                                        
088001     MOVE DAGENS-AA                       TO LIST-AAR                     
088102                                             E-LIST-AAR                   
088201     MOVE DAGENS-MM                       TO LIST-MAN                     
088302                                             E-LIST-MAN                   
088401     MOVE DAGENS-DD                       TO LIST-DAG                     
088502                                             E-LIST-DAG                   
088601                                                                          
088701     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
088803     .                                                                    
088903     EJECT                                                                
089003 B-INIT-HEADER-FOOTER SECTION.                                            
090003                                                                          
091003     MOVE IN-IDUSER    TO W-3148-IDUSER                                   
092003     MOVE IN-IDDC      TO W-3148-IDDC                                     
092004     MOVE IN-IDDC-REC  TO W-3148-IDDC-REC                                 
093003     PERFORM IMS-GU-WDGX3148                                              
093103     MOVE 3148-VLORDBTO          TO LIST-VLORDBTO-TOT                     
093203                                    E-LIST-VLORDBTO-TOT                   
093303     MOVE 'M³       '            TO LIST-UOM-VOLUME                       
093402     MOVE ' M³       '           TO E-LIST-UOM-VOLUME                     
093503                                                                          
093603     MOVE 3148-VKORDBTO          TO LIST-GROSS-WEIGHT                     
093703                                    E-LIST-GROSS-WEIGHT                   
093803     MOVE 'KG '                  TO LIST-UOM-WEIGHT                       
093903     MOVE ' KG '                 TO E-LIST-UOM-WEIGHT                     
094003                                                                          
094100     MOVE 3148-IDDC              TO LIST-IDDC-SND                         
094203                                    WS-IDDC-SND                           
094300                                    E-LIST-IDDC-SND                       
094403                                                                          
094503     MOVE IN-IDDC-REC            TO WS-IDDC                               
094605******************************************************************        
094704***  OK ATT ANVÄNDA 9927 DISTRIKT FÖR BÅDE DC11 OCH DC91 PGA              
094804***  STANDARDPRIS HÄMTAS FRÅN W335PRIS.                                   
094904     MOVE 9927                   TO SAVE-IDDISTR                          
095005******************************************************************        
095104     IF CDC-SE                                                            
095204        MOVE ZERO                TO LIST-IDDISTR                          
095304                                    E-LIST-IDDISTR                        
095404     ELSE                                                                 
095504        MOVE 9927                TO LIST-IDDISTR                          
095604                                    E-LIST-IDDISTR                        
095704     END-IF                                                               
095804                                                                          
095904     MOVE IN-IDDISTR             TO LIST-IDDISTR-SND                      
096004                                    E-LIST-IDDISTR-SND                    
096103                                    W-IDDISTR-2                           
097003                                                                          
098003     PERFORM BA-GET-CUSTOMER-INFO                                         
099003     .                                                                    
100003     EJECT                                                                
101003                                                                          
102003 BA-GET-CUSTOMER-INFO SECTION.                                            
103003                                                                          
104003     MOVE SPACE        TO  LIST-BEGMT-RAD1                                
104103                           LIST-BEGMT-RAD2                                
104203                           LIST-ADGMT-GATA                                
104303                           LIST-ADGMT-PADR                                
104403                           LIST-ADGMT-LAND                                
104503                           LIST-BEAVS-RAD1                                
104603                           LIST-BEAVS-RAD2                                
104703                           LIST-ADAVS-GATA                                
104803                           LIST-ADAVS-PADR                                
104903                           LIST-ADAVS-LAND                                
105003                                                                          
105103     MOVE SPACE        TO  E-LIST-BEGMT-RAD1                              
105203                           E-LIST-BEGMT-RAD2                              
105303                           E-LIST-ADGMT-GATA                              
105403                           E-LIST-ADGMT-PADR                              
105503                           E-LIST-ADGMT-LAND                              
105603                           E-LIST-BEAVS-RAD1                              
105703                           E-LIST-BEAVS-RAD2                              
105803                           E-LIST-ADAVS-GATA                              
105903                           E-LIST-ADAVS-PADR                              
106003                           E-LIST-ADAVS-LAND                              
106103                                                                          
106203     MOVE IN-IDDC-REC           TO W-IDDC                                 
106303     PERFORM IMS-GET-WDB601                                               
106403     IF SEGMENT-FINNS                                                     
106503       MOVE WDB6-DCS-BEGMT-RAD1 TO   LIST-BEGMT-RAD1                      
106603       MOVE WDB6-DCS-BEGMT-RAD2 TO   LIST-BEGMT-RAD2                      
106703       MOVE WDB6-DCS-ADGMT-GATA TO   LIST-ADGMT-GATA                      
106803       MOVE WDB6-DCS-ADGMT-PADR TO   LIST-ADGMT-PADR                      
106903       MOVE WDB6-DCS-ADGMT-LAND TO   LIST-ADGMT-LAND                      
107003                                                                          
107103       MOVE WDB6-DCS-BEGMT-RAD1 TO   E-LIST-BEGMT-RAD1                    
107203       MOVE WDB6-DCS-BEGMT-RAD2 TO   E-LIST-BEGMT-RAD2                    
107303       MOVE WDB6-DCS-ADGMT-GATA TO   E-LIST-ADGMT-GATA                    
107403       MOVE WDB6-DCS-ADGMT-PADR TO   E-LIST-ADGMT-PADR                    
107503       MOVE WDB6-DCS-ADGMT-LAND TO   E-LIST-ADGMT-LAND                    
107603     ELSE                                                                 
107703       MOVE 'MISSING '          TO   LIST-BEGMT-RAD1                      
107803       MOVE 'MISSING '          TO   LIST-BEGMT-RAD2                      
107903       MOVE 'MISSING '          TO   LIST-ADGMT-GATA                      
108003       MOVE 'MISSING '          TO   LIST-ADGMT-PADR                      
108103       MOVE 'MISSING '          TO   LIST-ADGMT-LAND                      
108203                                                                          
108303       MOVE 'MISSING '          TO   E-LIST-BEGMT-RAD1                    
108403       MOVE 'MISSING '          TO   E-LIST-BEGMT-RAD2                    
108503       MOVE 'MISSING '          TO   E-LIST-ADGMT-GATA                    
108603       MOVE 'MISSING '          TO   E-LIST-ADGMT-PADR                    
108703       MOVE 'MISSING '          TO   E-LIST-ADGMT-LAND                    
108803                                                                          
108903     END-IF                                                               
109003                                                                          
110003     MOVE  +0                    TO   WS-PAGE                             
111003     MOVE IN-IDDC                TO   W-IDDC                              
112003                                                                          
113003     PERFORM IMS-GET-WDB601                                               
114003     IF SEGMENT-FINNS                                                     
114103        MOVE WDB6-DCS-BEGMT-RAD1 TO   LIST-BEAVS-RAD1                     
114203        MOVE WDB6-DCS-BEGMT-RAD2 TO   LIST-BEAVS-RAD2                     
114303        MOVE WDB6-DCS-ADGMT-GATA TO   LIST-ADAVS-GATA                     
114403        MOVE WDB6-DCS-ADGMT-PADR TO   LIST-ADAVS-PADR                     
114503        MOVE WDB6-DCS-ADGMT-LAND TO   LIST-ADAVS-LAND                     
114603                                                                          
114703        MOVE WDB6-DCS-BEGMT-RAD1 TO   E-LIST-BEAVS-RAD1                   
114803        MOVE WDB6-DCS-BEGMT-RAD2 TO   E-LIST-BEAVS-RAD2                   
114903        MOVE WDB6-DCS-ADGMT-GATA TO   E-LIST-ADAVS-GATA                   
115003        MOVE WDB6-DCS-ADGMT-PADR TO   E-LIST-ADAVS-PADR                   
115103        MOVE WDB6-DCS-ADGMT-LAND TO   E-LIST-ADAVS-LAND                   
115203                                                                          
115303     ELSE                                                                 
115403        MOVE 'MISSING  '         TO   LIST-BEAVS-RAD1                     
115503        MOVE 'MISSING  '         TO   LIST-BEAVS-RAD2                     
115603        MOVE 'MISSING  '         TO   LIST-ADAVS-GATA                     
115703        MOVE 'MISSING  '         TO   LIST-ADAVS-PADR                     
115803        MOVE 'MISSING  '         TO   LIST-ADAVS-LAND                     
115903                                                                          
116003        MOVE 'MISSING  '         TO   E-LIST-BEAVS-RAD1                   
116103        MOVE 'MISSING  '         TO   E-LIST-BEAVS-RAD2                   
116203        MOVE 'MISSING  '         TO   E-LIST-ADAVS-GATA                   
116303        MOVE 'MISSING  '         TO   E-LIST-ADAVS-PADR                   
116403        MOVE 'MISSING  '         TO   E-LIST-ADAVS-LAND                   
116503                                                                          
116603     END-IF                                                               
116703     .                                                                    
116803     EJECT                                                                
116903                                                                          
117003 C-PROFORMA-HEADER SECTION.                                               
118003                                                                          
119003     MOVE RUBRIK-RAD TO WS-RAD                                            
120003     PERFORM S02-WRITE-UT2-RAD                                            
120103     MOVE SPACE TO WS-RAD                                                 
120203                                                                          
120303     ADD  +1      TO   WS-PAGE                                            
120403     MOVE WS-PAGE TO LIST-SIDA                                            
120503     MOVE U-RUBRIK-RAD1 TO WS-RAD                                         
120603     PERFORM S02-WRITE-UT2-RAD                                            
120703     MOVE SPACE TO WS-RAD                                                 
120803                                                                          
120903     MOVE U-RUBRIK-RAD2 TO WS-RAD                                         
121003     PERFORM S02-WRITE-UT2-RAD                                            
121103     MOVE SPACE TO WS-RAD                                                 
121203                                                                          
121303     MOVE U-RUBRIK-RAD3 TO WS-RAD                                         
121403     PERFORM S02-WRITE-UT2-RAD                                            
121503     MOVE SPACE TO WS-RAD                                                 
121603                                                                          
121703     MOVE U-RUBRIK-RAD4 TO WS-RAD                                         
121803     PERFORM S02-WRITE-UT2-RAD                                            
121903     MOVE SPACE TO WS-RAD                                                 
122003                                                                          
122103     MOVE U-RUBRIK-RAD5 TO WS-RAD                                         
122203     PERFORM S02-WRITE-UT2-RAD                                            
122303     MOVE SPACE TO WS-RAD                                                 
122403                                                                          
122503     MOVE U-RUBRIK-RAD6 TO WS-RAD                                         
122603     PERFORM S02-WRITE-UT2-RAD                                            
122703     MOVE SPACE TO WS-RAD                                                 
122803                                                                          
122903     MOVE U-RUBRIK-RAD6A TO WS-RAD                                        
123003     PERFORM S02-WRITE-UT2-RAD                                            
123103     MOVE SPACE TO WS-RAD                                                 
123203                                                                          
123303     MOVE U-RUBRIK-LINJE TO WS-RAD                                        
123403     PERFORM S02-WRITE-UT2-RAD                                            
123503     MOVE SPACE TO WS-RAD                                                 
123603                                                                          
123703     MOVE U-RUBRIK-RAD6C TO WS-RAD                                        
123803     PERFORM S02-WRITE-UT2-RAD                                            
123903     MOVE SPACE TO WS-RAD                                                 
124003                                                                          
125003     MOVE U-RUBRIK-RAD7 TO WS-RAD                                         
125103     PERFORM S02-WRITE-UT2-RAD                                            
125203     MOVE SPACE TO WS-RAD                                                 
125303                                                                          
125403     IF WRITE-HEADER                                                      
125503       MOVE E-RUBRIK-RAD TO UT-RAD                                        
125603       PERFORM S01-WRITE-UT-RAD                                           
125703       MOVE SPACE TO UT-RAD                                               
125803                                                                          
125903       MOVE E-U-RUBRIK-RAD1 TO UT-RAD                                     
126003       PERFORM S01-WRITE-UT-RAD                                           
126103       MOVE SPACE TO UT-RAD                                               
126203                                                                          
126303       MOVE E-U-RUBRIK-RAD2 TO UT-RAD                                     
126403       PERFORM S01-WRITE-UT-RAD                                           
126503       MOVE SPACE TO UT-RAD                                               
126603                                                                          
126703       MOVE E-U-RUBRIK-RAD3 TO UT-RAD                                     
126803       PERFORM S01-WRITE-UT-RAD                                           
126903       MOVE SPACE TO UT-RAD                                               
127003                                                                          
127103       MOVE E-U-RUBRIK-RAD4 TO UT-RAD                                     
127203       PERFORM S01-WRITE-UT-RAD                                           
127303       MOVE SPACE TO UT-RAD                                               
127403                                                                          
127503       MOVE E-U-RUBRIK-RAD5 TO UT-RAD                                     
127603       PERFORM S01-WRITE-UT-RAD                                           
127703       MOVE SPACE TO UT-RAD                                               
127803                                                                          
127903       MOVE E-U-RUBRIK-RAD6 TO UT-RAD                                     
128003       PERFORM S01-WRITE-UT-RAD                                           
128103       MOVE SPACE TO UT-RAD                                               
128203                                                                          
128303       MOVE E-U-RUBRIK-RAD6A TO UT-RAD                                    
128403       PERFORM S01-WRITE-UT-RAD                                           
128503       MOVE SPACE TO UT-RAD                                               
128603                                                                          
128703       MOVE E-U-RUBRIK-LINJE TO UT-RAD                                    
128803       PERFORM S01-WRITE-UT-RAD                                           
128903       MOVE SPACE TO UT-RAD                                               
129003                                                                          
129103       MOVE E-U-RUBRIK-RAD6C TO UT-RAD                                    
129203       PERFORM S01-WRITE-UT-RAD                                           
129303       MOVE SPACE TO UT-RAD                                               
129403                                                                          
129503       MOVE E-U-RUBRIK-RAD7 TO UT-RAD                                     
129603       PERFORM S01-WRITE-UT-RAD                                           
129703       MOVE SPACE TO UT-RAD                                               
129803       MOVE NOO TO WRITE-HEADER-SW                                        
129903     END-IF                                                               
130003     .                                                                    
130103     EJECT                                                                
130203 D-HANDLE-ROW SECTION.                                                    
130303     SKIP2                                                                
130403     PERFORM IMS-GNP-WDM611                                               
130503     IF SEGMENT-FINNS                                                     
130603       PERFORM DA-GET-PRICE                                               
130703       PERFORM DB-GET-DESCRIPTION                                         
130803       PERFORM IMS-GU-WDK611                                              
130903       MOVE WDK611-CLAG-VKART              TO WS-VKART                    
131003       COMPUTE WS-VKART-UNIT ROUNDED = WS-VKART / 1000                    
132003       COMPUTE WS-VKART-TOT ROUNDED =                                     
133003               WS-VKART-UNIT * WDM6-OBJ-KVRETUR-URSP                      
134003       COMPUTE WS-SUVKART-TOT =                                           
135003               WS-SUVKART-TOT + WS-VKART-TOT                              
135103       MOVE WS-VKART-TOT  TO LIST-VKART-TOT                               
135203                             E-LIST-VKART-TOT                             
135303       MOVE WDK611-CLAG-KDARTURS    TO LIST-KDARTURS                      
135403                                       E-LIST-KDARTURS                    
135503       MOVE WDK611-CLAG-IDSTATNR(3) TO LIST-IDSTATNR                      
135603                                       E-LIST-IDSTATNR                    
135703       MOVE WDM6-OBJ-IDARTNR-OBJ    TO LIST-IDARTNR                       
135803                                       E-LIST-IDARTNR                     
135903       MOVE WDM6-OBJ-KVRETUR-URSP   TO LIST-KVLEVART                      
136003                                       E-LIST-KVLEVART                    
136103       MOVE W-IDBYTRAP              TO LIST-IDBYTRAP                      
136203                                       E-LIST-IDBYTRAP                    
136303                                                                          
136403                                                                          
136503       MOVE U-RAD TO WS-RAD                                               
136603       PERFORM S02-WRITE-UT2-RAD                                          
136703       MOVE SPACE TO WS-RAD                                               
136803                                                                          
136903       MOVE E-U-RAD TO UT-RAD                                             
137003       PERFORM S01-WRITE-UT-RAD                                           
138003       MOVE SPACE TO UT-RAD                                               
139003     ELSE                                                                 
140003       MOVE YES TO REPORT-SW                                              
140103     END-IF                                                               
140203     .                                                                    
140303     EJECT                                                                
140403 DA-GET-PRICE SECTION.                                                    
140503                                                                          
140603                                                                          
140703     MOVE SAVE-IDDISTR                   TO PRIS-IDDISTR                  
140803     MOVE ZERO                           TO PRIS-IDKUNDNR                 
140900     MOVE 1                              TO PRIS-KDCALL                   
141000     MOVE IDPGM                          TO PRIS-IDPGM                    
141100     MOVE WDM6-OBJ-IDARTNR-OBJ           TO PRIS-IDARTNR                  
141203     MOVE WS-IDDC-SND                    TO PRIS-IDDC                     
141300     MOVE +4                             TO PRIS-KDORDKL                  
141400     MOVE +1                             TO PRIS-KVBEART                  
141500     MOVE SPACE                          TO PRIS-FLINVEST                 
141600                                                                          
141700     CALL W335PRIS USING PRIS-AREA                                        
141800                    PRIS-ARTC-PCB                                         
141900                    PRIS-WDK7-PCB                                         
142000                    PRIS-GMTA-PCB                                         
142100                    PRIS-BETA-PCB                                         
142200                    PRIS-GPRIA-PCB                                        
142300                    PRIS-GPRIB-PCB                                        
142400                    PRIS-COST-WDK6-PCB                                    
142500                    PRIS-COST-WDK7-PCB                                    
142600                    PRIS-COST-WDF1-PCB                                    
142700                    PRIS-COST-9305-PCB                                    
142800                    PRIS-COST-WDK72-PCB                                   
142900                    PRIS-COST-WDB6-PCB                                    
143000                                                                          
143103     MOVE PRIS-PRARTSTD TO WS-PRFKTUTL                                    
143203     MOVE PRIS-KDVALISO TO LIST-KDVALISO                                  
143303                           E-LIST-KDVALISO                                
143403                                                                          
143503     COMPUTE WS-PRFKTUTL-ROW = WS-PRFKTUTL *                              
143603                               WDM6-OBJ-KVRETUR-URSP                      
143703                                                                          
143803                                                                          
143903     MOVE WS-PRFKTUTL-ROW              TO LIST-PRFKTUTL-RAD               
144003                                          E-LIST-PRFKTUTL-RAD             
144103     ADD  WS-PRFKTUTL-ROW              TO WS-SUFKTUTL-TOT                 
144203     .                                                                    
144303     EJECT                                                                
144403 DB-GET-DESCRIPTION SECTION.                                              
144503     SKIP2                                                                
144603     MOVE WDM6-OBJ-IDARTNR-OBJ TO W-IDARTNR                               
144703     PERFORM IMS-GU-WDD311                                                
144803     IF SEGMENT-FINNS                                                     
144903       MOVE WDD311-TEXT-BEART TO LIST-BEART                               
145003                                 E-LIST-BEART                             
146003     ELSE                                                                 
147003       MOVE SPACE             TO LIST-BEART                               
148003                                 E-LIST-BEART                             
149003     END-IF                                                               
150003     .                                                                    
160003     EJECT                                                                
160103 F-CREATE-LASTPAGE SECTION.                                               
160203     SKIP2                                                                
160303     MOVE WS-SUFKTUTL-TOT  TO LIST-SUFKTUTL-TOT                           
160403                              E-LIST-SUFKTUTL-TOT                         
160503     MOVE WS-SUVKART-TOT   TO LIST-SUVKART-TOT                            
160603                              E-LIST-SUVKART-TOT                          
160703     MOVE 'KG '            TO LIST-UOM-TWEIGHT                            
160803     MOVE ' KG '           TO E-LIST-UOM-TWEIGHT                          
160903                                                                          
161003     MOVE E-U-RUBRIK-LINJE TO UT-RAD                                      
162003     PERFORM S01-WRITE-UT-RAD                                             
162103     MOVE SPACE TO UT-RAD                                                 
162203                                                                          
162303     MOVE SISTA-RADEN1    TO WS-RAD                                       
162403     MOVE E-SISTA-RADEN1  TO UT-RAD                                       
162503     PERFORM S02-WRITE-UT2-RAD                                            
162603     PERFORM S01-WRITE-UT-RAD                                             
162703     MOVE SPACE TO WS-RAD                                                 
162803                   UT-RAD                                                 
162903                                                                          
163003     MOVE SISTA-RADEN2    TO WS-RAD                                       
163103     MOVE E-SISTA-RADEN2  TO UT-RAD                                       
163203     PERFORM S02-WRITE-UT2-RAD                                            
163303     PERFORM S01-WRITE-UT-RAD                                             
163403     MOVE SPACE TO WS-RAD                                                 
163503                   UT-RAD                                                 
163603                                                                          
163703     MOVE SISTA-RADEN3   TO WS-RAD                                        
163803     MOVE E-SISTA-RADEN3 TO UT-RAD                                        
163903     PERFORM S02-WRITE-UT2-RAD                                            
164003     PERFORM S01-WRITE-UT-RAD                                             
165003     MOVE SPACE          TO WS-RAD                                        
165103                            UT-RAD                                        
165203                                                                          
165303     MOVE SISTA-RADEN4    TO WS-RAD                                       
165403     MOVE E-SISTA-RADEN4  TO UT-RAD                                       
165503     PERFORM S02-WRITE-UT2-RAD                                            
165603     PERFORM S01-WRITE-UT-RAD                                             
165703     MOVE SPACE           TO WS-RAD                                       
165803                             UT-RAD                                       
165903                                                                          
166003     MOVE SISTA-RADEN5    TO WS-RAD                                       
166103     MOVE E-SISTA-RADEN5  TO UT-RAD                                       
166203     PERFORM S02-WRITE-UT2-RAD                                            
166303     PERFORM S01-WRITE-UT-RAD                                             
166403     MOVE SPACE           TO WS-RAD                                       
166503                             UT-RAD                                       
166603                                                                          
166703     MOVE SISTA-RADEN6    TO WS-RAD                                       
166803     MOVE E-SISTA-RADEN6  TO UT-RAD                                       
166903     PERFORM S02-WRITE-UT2-RAD                                            
167003     PERFORM S01-WRITE-UT-RAD                                             
167103     MOVE SPACE           TO WS-RAD                                       
167203                          UT-RAD                                          
167303     .                                                                    
167403     EJECT                                                                
167503 Z-FINIT SECTION.                                                         
167603                                                                          
167703     PERFORM S02-WRITE-UT2-RAD                                            
167803     CLOSE INDATA                                                         
167903           W37151                                                         
168003           W37152                                                         
169003     MOVE 'S' TO POSTSUM-OPKOD                                            
170003     CALL POSTSUM USING POSTSUM-PARM                                      
180003     .                                                                    
181003 S01-WRITE-UT-RAD   SECTION.                                              
182003     WRITE UTPOST FROM UT-RAD                                             
183003     MOVE 'UT-'        TO POSTSUM-TRANSTYP                                
184003     MOVE 'W37151 ' TO POSTSUM-FDNAMN                                     
184103     MOVE 'W37158D2' TO POSTSUM-DDNAMN2                                   
184203     CALL POSTSUM USING POSTSUM-PARM                                      
184303     .                                                                    
184403 S02-WRITE-UT2-RAD  SECTION.                                              
184503     WRITE UTPOST2 FROM WS-RAD                                            
184603     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
184703     MOVE 'W37152 '  TO POSTSUM-FDNAMN                                    
184803     MOVE 'W37158D3' TO POSTSUM-DDNAMN2                                   
184903     CALL POSTSUM USING POSTSUM-PARM                                      
185003     .                                                                    
185103     EJECT                                                                
185203* --- IMS SEKTIONER ---                                                   
185303     SKIP3                                                                
185403     EJECT                                                                
185503 IMS-GET-WDB601 SECTION.                                                  
185603     MOVE 'IMS-GET-WDB601' TO WS-IMS-SEKTION                              
185703                                                                          
185803     STRING 'WDB601  (IDDC     =' W-WDB601-X ')'                          
185903          DELIMITED BY SIZE INTO SSA1                                     
186003     MOVE '  GE' TO GODK-STATUSKODER                                      
186103     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
186203     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
186303     PERFORM IMS-STATUSKONTROLL                                           
186403     .                                                                    
186503     EJECT                                                                
186603 IMS-GU-WDK611 SECTION.                                                   
186703     MOVE 'IMS-GU-WDK611' TO WS-IMS-SEKTION                               
186803                                                                          
186903     SKIP2                                                                
187003     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
188003           DELIMITED BY SIZE INTO SSA1                                    
189003     MOVE 'WDK611   ' TO SSA2                                             
189103     MOVE '  ' TO GODK-STATUSKODER                                        
189203     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
189303     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
189403     PERFORM IMS-STATUSKONTROLL                                           
189503     .                                                                    
189603     EJECT                                                                
189703 IMS-GU-WDGX3148 SECTION.                                                 
189803                                                                          
189903     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
190003             DELIMITED BY SIZE INTO SSA1                                  
190103     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
190203             DELIMITED BY SIZE INTO SSA2                                  
190303     MOVE '  '                   TO GODK-STATUSKODER                      
190403     CALL CBLTDLI USING GU 3147-PCB DLI-IO-WDGX3148 SSA1 SSA2             
190503     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
190603     PERFORM IMS-STATUSKONTROLL                                           
190703     .                                                                    
190803     EJECT                                                                
190903 IMS-GNP-WDGX3150 SECTION.                                                
191003                                                                          
191103     STRING 'WDGX3150(IDDISTR  =' W-IDDISTR-X ')'                         
191203     DELIMITED BY SIZE INTO SSA1                                          
191303     MOVE '  GEGB'               TO GODK-STATUSKODER                      
191403     CALL CBLTDLI USING GNP 3147-PCB DLI-IO-WDGX3150 SSA1                 
191503     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
191603     PERFORM IMS-STATUSKONTROLL                                           
191703     .                                                                    
191803     EJECT                                                                
191903 IMS-GU-WDD311 SECTION.                                                   
192003     MOVE 'IMS-GU-WDD311'      TO WS-IMS-SEKTION                          
193003     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
194003          DELIMITED BY SIZE INTO SSA1                                     
195003     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
196003          DELIMITED BY SIZE INTO SSA2                                     
197003     MOVE '  GE' TO GODK-STATUSKODER                                      
198003     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
198103     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
198203     PERFORM IMS-STATUSKONTROLL                                           
198303     .                                                                    
198403     EJECT                                                                
198503 IMS-GU-WDM601 SECTION.                                                   
198603     MOVE 'IMS-GU-WDM601' TO WS-IMS-SEKTION                               
198703                                                                          
198803     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
198903          DELIMITED BY SIZE INTO SSA1                                     
199003     MOVE '  ' TO GODK-STATUSKODER                                        
199103     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
199203     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
199303     PERFORM IMS-STATUSKONTROLL                                           
199403     .                                                                    
199503     EJECT                                                                
199603 IMS-GNP-WDM611 SECTION.                                                  
199703     MOVE 'IMS-GNP-WDM611' TO WS-IMS-SEKTION                              
199803                                                                          
199903     MOVE 'WDM611' TO SSA1                                                
200003     MOVE '  GEGB' TO GODK-STATUSKODER                                    
200100     CALL CBLTDLI USING GNP WDM6-PCB DLI-IO-WDM611 SSA1                   
200200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200503     EJECT                                                                
200603 IMS-STATUSKONTROLL SECTION.                                              
200703     SKIP2                                                                
200803     SET STATUS-IX TO 1                                                   
200903     SEARCH GODK-STATUS                                                   
201003       AT END                                                             
201103         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
201203           DELIMITED BY SIZE INTO ERRTEXT                                 
201303         DISPLAY ERRTEXT                                                  
201403         CALL FELLOG                                                      
201503       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
201603         CONTINUE                                                         
201703     END-SEARCH                                                           
201803     .                                                                    
