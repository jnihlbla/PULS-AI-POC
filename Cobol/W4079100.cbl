000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4079100.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000400 DATE-WRITTEN.   95/03/20.                                                
000500*                                                                         
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP                                   
001000*        LÄGGER UPP LEVERANSANMÄRKNINGAR PÅ KREDITERINGSREGISTRET         
001100*                                                                         
001200*        FÖR AUTOMATGODKÄNDA LEV.ANM. HÄMTAR MAN ATTESTANSVARIG           
001300*        PERSONS ID + NAMN FRÅN WDGX4126.DETTA NAMN + ID SKALL            
001400*        SKRIVAS UT PÅ KREDITNOTAN.SAKNAS DISTRIKTET SÅ SKALL             
001500*        MAN ANVÄNDA DISTRIKT 9999.OBS! USA/JAP/AUS UNDANTAGNA.           
001600*                                                                         
001700*        PROGRAMMET LÄSER WDGX4124 (WDR1)                                 
001800*                         WDGX4126                                        
001900*                                                                         
002000*    E-TRACKER 1572353 DATUM 20050323                                     
002100*    E-TRACKER 2913019 DATUM 20051209                                     
002200*    E-TRACKER 1658417 DATUM 20060404                                     
002300*    E-TRACKER 850114  DATUM 20070404                                     
002400*    E-TRACKER 5276159 DATUM 20070815                                     
002500*    E-TRACKER 8635407 DATUM 20091021 RETURN CODES MATRIX                 
002600*    E-TRACKER 9292454 DATUM 20100316 LEV.ANM.STATUS=3 OCH Q/FEL          
002700*    E-TRACKER 10143271 DATUM 20111122 CHINA WAREHOUSE PROJECT-1          
002800*    E-TRACKER 10143273 DATUM 20121130 RÄTTA BILD 4717 SUKRENOT           
002900*                                      VIPS-FAKTURA PRIS.                 
003000*    E-TRACKER 8198803 DATUM 20170901 AUTO REM FÖR VISSA KODER.           
003100*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
003200*    STORY 2583419 NEW JP NDC-6A                                          
003301*    STORY         ADAPT. TO THE NEW WDL5                                 
003401*                                                                         
003501*    INDATA.                                                              
003601*        TRANSAKTION: W4T791X                                             
003701*                                                                         
003801                                                                          
003901     SKIP3                                                                
004001 ENVIRONMENT DIVISION.                                                    
004101     EJECT                                                                
004201 DATA DIVISION.                                                           
004301 WORKING-STORAGE SECTION.                                                 
004401*    -- CHECKED BY WY2000                                                 
004501     SKIP3                                                                
004601 77  IDPGM                       PIC X(08)   VALUE 'W4079100'.            
004701                                                                          
004801*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004901 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005001 77  KDRC-DISPLAY                PIC Z(5)  VALUE ZERO.                    
005101                                                                          
005201 77  JA                          PIC X      VALUE 'J'.                    
005301 77  YES                         PIC X      VALUE 'Y'.                    
005401 77  NEJ                         PIC X      VALUE 'N'.                    
005501 77  IX                          PIC S9(3)  VALUE +0    COMP SYNC.        
005601 77  MAX-IX                      PIC S9(3)  VALUE +0    COMP SYNC.        
005701 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005801 77  WS-IDRADNR                  PIC 9(4)   VALUE ZERO.                   
005901 77  WS-IDPRQUES                 PIC 9(7)   VALUE ZERO.                   
006001 77  WS-ANTAL-SEND               PIC S9(7)  VALUE +0   COMP-3.            
006101 77  WS-SUKRENOT                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006201 77  WS-PRARTBTO                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006301 77  WS-PRARTBTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006401 77  WS-KDVALISO                 PIC X(3)   VALUE SPACE.                  
006501 77  WS-KDKRENOT                 PIC X(2)   VALUE SPACE.                  
006601 77  WS-IDDC-RET                 PIC X(2)   VALUE SPACE.                  
006701 77  WS-IXDCCLEAR                PIC 9      VALUE ZERO.                   
006801 77  WS-MATRIX-Q                 PIC X      VALUE 'N'.                    
006901                                                                          
007001 77  R5-UPD-SW                   PIC X.                                   
007101   88  WDR501-SAKNAS                        VALUE 'N'.                    
007201   88  WDR501-FINNS                         VALUE 'J'.                    
007301                                                                          
007401 77  WDB201-SW                   PIC X.                                   
007501   88  WDB201-SAKNAS                        VALUE 'N'.                    
007601   88  WDB201-FINNS                         VALUE 'J'.                    
007701                                                                          
007801 77  ALLT-SW                     PIC X.                                   
007901     88  ALLT-OK                             VALUE 'J'.                   
008001     88  ALLT-FEL                            VALUE 'N'.                   
008101                                                                          
008201*-KODERNA 52, 53 OCH 72 AUTOMATGODKÄNNS.                                  
008301*-AND DDGS ALSO FOR 20,21,92 BUT IDFTG= 57 AND DC NOT 61,62               
008401 77  AUTO-KOD-SW                 PIC X.                                   
008501     88  BARA-AUTO-KOD                       VALUE 'J'.                   
008601     88  INTE-BARA-AUTO-KOD                  VALUE 'N'.                   
008701                                                                          
008801*-VISSA KODER GÅR ATOMATISKT TILL REMISS.                                 
008901 77  AUTO-REM-SW                 PIC X.                                   
009001     88  AUTO-REM-OK                         VALUE 'J'.                   
009101                                                                          
009201*                                                                         
009301 77  UPPDAT-STATUS-2-SW          PIC X       VALUE 'N'.                   
009401     88  UPPDAT-STATUS-2-OK                  VALUE 'J'.                   
009501                                                                          
009601 77  NYCKLAR-SW                  PIC X.                                   
009701     88  NYCKLAR-OK                          VALUE 'J'.                   
009801     88  NYCKLAR-FEL                         VALUE 'N'.                   
009901                                                                          
010001 77  HAEMTA-PRIS-SW              PIC X       VALUE 'N'.                   
010101     88  HAEMTA-PRIS-VIPS                    VALUE 'J'.                   
010201                                                                          
010301 77  HAEMTA-USER-SW              PIC X       VALUE 'N'.                   
010401     88  HAEMTA-USER-OK                      VALUE 'J'.                   
010501                                                                          
010601 77  ATTEST-KNOTA-SW             PIC X       VALUE 'N'.                   
010701     88  ATTEST-KNOTA-KRAV                   VALUE 'J'.                   
010801                                                                          
010901 01  AUTO-APPROVAL-SW            PIC X       VALUE 'Y'.                   
011001     EJECT                                                                
011101*      --- VALID IDDC CODES                                               
011201*                                                                         
011301*01    -COPY WWDC99                                                       
011401       EJECT                                                              
011501 01  FILLER                      PIC X(80)   VALUE ALL 'A'.               
011601*                                                                         
011701 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
011801*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
011901     EJECT                                                                
012001*01  FILLER  -COPY WWDIST03 -RED TEST-IDDISTR.                            
012101                                                                          
012201     EJECT                                                                
012301 01  MESSAGE-CODES.                                                       
012401     03  ERR-OTILL-UPPDAT        PIC X(3)    VALUE '007'.                 
012501     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
012601     EJECT                                                                
012701 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
012801 01  NYCKLAR.                                                             
012901   03  W-IDLEVANM-X.                                                      
013001     05  W-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
013101     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO  COMP-3.          
013201     05  W-IDRAPPNR              PIC  9(7).                               
013301                                                                          
013401   03 W-IDGMT-X.                                                          
013501     05 W-IDDISTR-WDB2           PIC S9(5)   COMP-3.                      
013601     05 W-IDKUNDNR-WDB2          PIC S9(7)   COMP-3.                      
013701                                                                          
013801   03 W-WDB101KY-X.                                                       
013901     05 W-IDPARTNR               PIC X(9).                                
014001     05 W-IDFTG                  PIC 9(2).                                
014101                                                                          
014201   03  W-WDC701KY-X.                                                      
014301     05  W-PRQ-IDDISTR           PIC 9(4)  VALUE ZERO.                    
014401     05  W-PRQ-IDKUNDNR          PIC 9(7)  VALUE ZERO.                    
014501     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
014601     05  W-PRQ-IDORDNR7-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
014701       07  W-PRQ-IDORDNR7        PIC 9(7).                                
014801       07  FILLER                PIC X(8).                                
014901     05  W-PRQ-IDRAPPNR-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
015001       07  W-PRQ-IDRAPPNR        PIC 9(7).                                
015101       07  FILLER                PIC X(8).                                
015201                                                                          
015301   03  W-WDC711KY-X.                                                      
015401     05  W-LPRQ-IDPRQUES         PIC 9(7)  VALUE ZERO.                    
015501                                                                          
015601   03  W-WDGXKEY-4123-X.                                                  
015701     05  W-IDHTYP-4123           PIC  X(4)   VALUE '4123'.                
015801     05  FILLER                  PIC  X(26)  VALUE LOW-VALUE.             
015901                                                                          
016001   03  W-IDDISTR-X.                                                       
016101     05  W-IDDISTR-4124          PIC S9(5) VALUE ZERO COMP-3.             
016201                                                                          
016301   03  W-KEY4124-X.                                                       
016401     05  W-4124-IDDISTR-FOM      PIC S9(5) VALUE ZERO COMP-3.             
016501     05  W-4124-IDDISTR-TOM      PIC S9(5) VALUE ZERO COMP-3.             
016601                                                                          
016701   03 W-KEY4126-X.                                                        
016801     05  W-4126-SUKRENOT-FOM     PIC S9(7) VALUE ZERO COMP-3.             
016901     05  W-4126-SUKRENOT-TOM     PIC S9(7) VALUE ZERO COMP-3.             
017001                                                                          
017101   03  W-WDGXKEY-4103-X.                                                  
017201     05  W-IDHTYP-4103           PIC X(4)  VALUE '4103'.                  
017301     05  W-IDDISTR-4103          PIC S9(5) VALUE ZERO COMP-3.             
017401     05  W-IDKUNDNR-4103         PIC S9(7) VALUE ZERO COMP-3.             
017501     05  W-IDRAPPNR-4103         PIC 9(7)  VALUE ZERO.                    
017601     05  FILLER                  PIC X(12) VALUE LOW-VALUE.               
017701                                                                          
017801   03  W-KEY4104-X.                                                       
017901     05  W-IDDC-4104             PIC X(2)  VALUE SPACE.                   
018001     05  W-KDKRENOT-4104         PIC X(2)  VALUE SPACE.                   
018101                                                                          
018201                                                                          
018306   03  W-IDFAKT-X.                                                        
018402     05  W-IDFAKT                PIC S9(7)   COMP-3 VALUE ZERO.           
018603                                                                          
020402   03  W-IDGMTREF-X.                                                      
020502     05  W-IDDISTR-L5            PIC S9(5)   COMP-3 VALUE ZERO.           
020602     05  W-IDKUNDNR-L5           PIC S9(7)   COMP-3 VALUE ZERO.           
020702     05  W-IDKUNDRF-L5           PIC X(10).                               
020802                                                                          
021004   03  W-IDARTNR-X.                                                       
021104     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
021202                                                                          
021305   03  W-WDL511KY-X.                                                      
021405     05 W-IDPRODNR               PIC S9(7)   VALUE ZERO  COMP-3.          
021505     05 W-IDKOLLI-X.                                                      
021605       07 W-IDKOLLI              PIC S9(5)   VALUE ZERO  COMP-3.          
021705                                                                          
021902   03  W-WDGXKEY-4129-X.                                                  
022002       05  W-IDHTYP-4129         PIC  X(4)   VALUE '4129'.                
022102       05  FILLER                PIC  X(26)  VALUE LOW-VALUE.             
022202                                                                          
022302   03  W-WDGXKEY-4130-X.                                                  
022402       05  W-4130-IDDC           PIC X(2)    VALUE SPACE.                 
022502                                                                          
022602   03  W-WDGXKEY-4132-X.                                                  
022702       05  W-4132-KDANMORS       PIC X(2)    VALUE SPACE.                 
022802                                                                          
022902   77  W-KDKUNDKAT               PIC 9(2)    VALUE ZERO.                  
023002                                                                          
023102******************************************************************        
023202     EJECT                                                                
023302 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
023402     SKIP3                                                                
023502 01  -COPY WZ01SEND                                                       
023602     EJECT                                                                
023702 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
023802     SKIP3                                                                
023902 01  SEND-AREA.                                                           
024002*    03  -COPY WZ01REQU  -PRE 3391-                                       
024102*    03  -COPY W30391I1  -PRE 3391-                                       
024202     EJECT                                                                
024302                                                                          
024402     SKIP2                                                                
024502 01  RETURKODER.                                                          
024602     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
024702     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
024802                                                                          
024902     EJECT                                                                
025002*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
025102 01  GENERELLA-SUBPROGRAM.                                                
025202     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025302     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025402     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025502     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
025602     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
025702     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
025802     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
025902     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
026002     EJECT                                                                
026102 01  FILLER                      PIC X(16)   VALUE 'DECAREA '.            
026202     SKIP3                                                                
026302 01  DECAREA.                                                             
026402* 03  WDECAREA   -COPY WDECAREA                                           
026502     EJECT                                                                
026602*    --- PARAMETRAR TILL SUBPROGRAM W335PRNO                              
026702*   -COPY W335PRNO                                                        
026802     EJECT                                                                
026902*    ---  LÄNKAREA TILL W418OKOD                                          
027002 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
027102*01 -COPY W418OKOD           -PRE OKOD-.                                  
027202     EJECT                                                                
027302 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
027402 01  KOM-IO-AREA.                                                         
027502*03  -COPY WMSGKOM                                                        
027602     EJECT                                                                
027702*                                                                         
027802 01  FILLER                      PIC X(80)   VALUE ALL 'B'.               
027902*                                                                         
028002 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
028102     SKIP3                                                                
028202*01  -COPY W4I79101                                                       
028302     EJECT                                                                
028402 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
028502     SKIP3                                                                
028602*01  -COPY WMSGAREA                                                       
028702     EJECT                                                                
028802*01  -COPY WWIDFTG                                                        
028902     EJECT                                                                
029002*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029102*                                                                         
029202 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029302     SKIP3                                                                
029402 01  FILLER                      PIC X(80)   VALUE ALL 'C'.               
029502                                                                          
029602*    --- STATUS-KOD FRÅN IMS                                              
029702 01  STATUS-WS                   PIC XX.                                  
029802     88  SEGMENT-FINNS                       VALUE '  '.                  
029902     88  SEGMENT-REDAN-FINNS                 VALUE 'II'.                  
030002     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030102     88  SEGMENT-SLUT                        VALUE 'GB'.                  
030202     SKIP2                                                                
030302 01  GODK-STATUSKODER.                                                    
030402     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030502     SKIP3                                                                
030602 01  SSA1                        PIC X(128).                              
030702 01  SSA2                        PIC X(64).                               
030802 01  SSA3                        PIC X(64).                               
030902     EJECT                                                                
031002 01  FILLER                      PIC X(80)   VALUE ALL 'D'.               
031102*    --- IMS FUNKTIONSKODER                                               
031202*01  -COPY W0003                                                          
031302*                                                                         
031402 01  DLI-IO-AREA.                                                         
031502   03  IO-AREA                   PIC X(900)  VALUE SPACE.                 
031602     SKIP3                                                                
031702*  03  WDA201    -COPY WDA201  -RED IO-AREA.                              
031802     EJECT                                                                
031902*  03  WDA211    -COPY WDA211  -RED IO-AREA.                              
032002     EJECT                                                                
032102 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-2'.        
032202                                                                          
032302 01  DLI-IO-AREA-WDB1.                                                    
032402*  03  WDB101  -COPY WDB101                                               
032502     EJECT                                                                
032602 01  DLI-IO-AREA-WDB2.                                                    
032702*  03  WDB201    -COPY WDB201                                             
032802     EJECT                                                                
032902 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
033002 01  DLI-IO-WDC701.                                                       
033102*    03  -COPY WDC701                                                     
033202                                                                          
033302 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
033402 01  DLI-IO-WDC711.                                                       
033502*    03  -COPY WDC711                                                     
033602     EJECT                                                                
033702 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR101'.                      
033802 01  DLI-IO-WDR101.                                                       
033902*    03  -COPY WDGX01                                                     
034002     EJECT                                                                
034102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4124'.                    
034202 01  DLI-IO-WDGX4124.                                                     
034302*    03  -COPY WDGX4124                                                   
034402     EJECT                                                                
034502 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4126'.                    
034602 01  DLI-IO-WDGX4126.                                                     
034702*    03  -COPY WDGX4126                                                   
034802     EJECT                                                                
034902 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
035002 01  DLI-IO-WDGX4103.                                                     
035102*    03  -COPY WDGX4103                                                   
035202     EJECT                                                                
035302 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
035402 01  DLI-IO-WDGX4104.                                                     
035502*    03  -COPY WDGX4104                                                   
035602 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL501'.                      
035702 01  DLI-IO-WDL501.                                                       
035802*    03  -COPY WDL501                                                     
035902     EJECT                                                                
036003 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL511'.                      
036103 01  DLI-IO-WDL511.                                                       
036203*    03  -COPY WDL511                                                     
036303     EJECT                                                                
036403 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL521'.                      
036503 01  DLI-IO-WDL521.                                                       
036603*    03  -COPY WDL521                                                     
036703     EJECT                                                                
036803 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
036903 01  DLI-IO-WDR201.                                                       
037003*    03  -COPY WDGX01                                                     
037103*                                                                         
037203 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4130'.                    
037303 01  DLI-IO-WDGX4130.                                                     
037403 *    03  -COPY WDGX4130.                                                 
037503*                                                                         
037603 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4132'.                    
037703 01  DLI-IO-WDGX4132.                                                     
037803 *    03  -COPY WDGX4132.                                                 
037903     EJECT                                                                
038003 01  FILLER                      PIC X(80)   VALUE ALL 'E'.               
038103*    --- LÄNKAREA TILL W418ANSV                                           
038203*01  -COPY W418ANSV                                                       
038303     EJECT                                                                
038403 LINKAGE SECTION.                                                         
038503                                                                          
038603*01  -COPY W0009  -PRE MSG-                                               
038703     EJECT                                                                
038803*01  -COPY W0009  -PRE DISP-                                              
038903     EJECT                                                                
039003*01  -COPY W0009  -PRE PRQRY-                                             
039103     EJECT                                                                
039203*01  -COPY W0008  -PRE WDA2-                                              
039303     05  FILLER                  PIC X.                                   
039403     EJECT                                                                
039503*01  -COPY W0008  -PRE 4113-                                              
039603     05  FILLER                  PIC X.                                   
039703     EJECT                                                                
039803*01  -COPY W0008  -PRE WDB1-                                              
039903     05  FILLER                  PIC X.                                   
040003     EJECT                                                                
040103*01  -COPY W0008  -PRE WDB2-                                              
040203     05  FILLER                  PIC X.                                   
040303     EJECT                                                                
040403*01  -COPY W0008  -PRE 4115-                                              
040503     05  FILLER                  PIC X.                                   
040603     EJECT                                                                
040703*01  -COPY W0008  -PRE 4117-                                              
040803     05  FILLER                  PIC X.                                   
040903     EJECT                                                                
041003*01  -COPY W0008  -PRE WDC7-                                              
041103     05  FILLER                  PIC X.                                   
041203     EJECT                                                                
041303 01  PRNO-PCB                    PIC X.                                   
041403     EJECT                                                                
041503*01  -COPY W0008  -PRE 4123-                                              
041603     05  FILLER                  PIC X.                                   
041703     EJECT                                                                
041803*01  -COPY W0008  -PRE 4103-                                              
041903     05  FILLER                  PIC X.                                   
042003     EJECT                                                                
042103*01  -COPY W0008  -PRE WDL5-                                              
042203     05  FILLER                  PIC X.                                   
042303     EJECT                                                                
042403*01  -COPY W0008   -PRE 4129-                                             
042503     05  FILLER                  PIC X.                                   
042603     EJECT                                                                
042703 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB PRQRY-PCB WDA2-PCB            
042803                           4113-PCB WDB1-PCB WDB2-PCB 4115-PCB            
042903                           4117-PCB WDC7-PCB PRNO-PCB 4123-PCB            
043003                           4103-PCB WDL5-PCB 4129-PCB.                    
043103 MAIN SECTION.                                                            
043203     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB PRQRY-PCB WDA2-PCB            
043303                           4113-PCB WDB1-PCB WDB2-PCB 4115-PCB            
043403                           4117-PCB WDC7-PCB PRNO-PCB 4123-PCB            
043503                           4103-PCB WDL5-PCB 4129-PCB.                    
043603                                                                          
043703     PERFORM IMS-GET-MSG                                                  
043803                                                                          
043903     IF SEGMENT-FINNS                                                     
044003       PERFORM IMS-GN-MSG                                                 
044103     END-IF                                                               
044203                                                                          
044303     IF SEGMENT-FINNS                                                     
044403       PERFORM A-INIT                                                     
044503       IF ALLT-OK                                                         
044603          PERFORM B-KOLLA-INDATA                                          
044703          IF NYCKLAR-OK                                                   
044803             PERFORM C-BEHANDLA                                           
044903             MOVE SPACE                  TO MSG-KOM-IDMFSMED              
045003          ELSE                                                            
045103             MOVE 'F'                    TO MSG-KOM-IDMFSMED              
045203             MOVE '4'                    TO MSG-KOM-KDSVAR                
045303          END-IF                                                          
045403       ELSE                                                               
045503          MOVE 'F'                       TO MSG-KOM-IDMFSMED              
045603          MOVE '4'                       TO MSG-KOM-KDSVAR                
045703       END-IF                                                             
045803       PERFORM Z-FINIT                                                    
045903     END-IF                                                               
046003                                                                          
046103                                                                          
046203     MOVE ZERO TO RETURN-CODE                                             
046303     GOBACK                                                               
046403     .                                                                    
046503     EJECT                                                                
046603 A-INIT SECTION.                                                          
046703                                                                          
046803     MOVE JA    TO ALLT-SW                                                
046903     MOVE JA    TO NYCKLAR-SW                                             
047003     MOVE JA    TO AUTO-KOD-SW                                            
047103     MOVE NEJ   TO AUTO-REM-SW                                            
047203     MOVE NEJ   TO UPPDAT-STATUS-2-SW                                     
047303     MOVE NEJ   TO HAEMTA-USER-SW                                         
047403     MOVE ZERO  TO WS-IDPRQUES                                            
047503     MOVE NEJ   TO ATTEST-KNOTA-SW                                        
047603     MOVE ZERO  TO WS-IXDCCLEAR                                           
047703     MOVE SPACE TO WS-IDDC-RET                                            
047803     MOVE NEJ   TO WS-MATRIX-Q                                            
047903                                                                          
048003     MOVE SPACE                          TO MID-W4I79101                  
048103     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I79101                  
048203                                                                          
048303     IF MSG-KDTRANS-1  NOT = 'W4T791X '                                   
048403        MOVE NEJ TO ALLT-SW                                               
048503     END-IF                                                               
048603     .                                                                    
048703     EJECT                                                                
048803 B-KOLLA-INDATA SECTION.                                                  
048903     IF MID-IDDISTR NOT NUMERIC                                           
049003        MOVE NEJ                         TO NYCKLAR-SW                    
049103     END-IF                                                               
049203                                                                          
049303     IF MID-IDKUNDNR NOT NUMERIC                                          
049403        MOVE NEJ                         TO NYCKLAR-SW                    
049503     END-IF                                                               
049603     .                                                                    
049703     EJECT                                                                
049803 C-BEHANDLA SECTION.                                                      
049903                                                                          
050003     MOVE MID-IDDISTR                    TO W-IDDISTR                     
050103                                            W-IDDISTR-WDB2                
050203                                            TEST-IDDISTR                  
050303                                            W-PRQ-IDDISTR                 
050403                                            W-IDDISTR-4103                
050503     MOVE MID-IDKUNDNR                   TO W-IDKUNDNR                    
050603                                            W-IDKUNDNR-WDB2               
050703                                            W-PRQ-IDKUNDNR                
050803                                            W-IDKUNDNR-4103               
050903     MOVE MID-IDRAPPNR                   TO W-IDRAPPNR                    
051003                                            W-PRQ-IDRAPPNR                
051103                                            W-IDRAPPNR-4103               
051203                                                                          
051303                                                                          
051403     IF MID-IDSYSTEM = 'STA'                                              
051503       IF MID-LEVANM-KLAR = JA OR YES                                     
051603                                                                          
051703          PERFORM IMS-GU-WDA2-ANM                                         
051803          IF SEGMENT-FINNS                                                
051903            PERFORM IMS-GNP-WDA2-LEV                                      
052003            PERFORM UNTIL SEGMENT-SAKNAS                                  
052103              OR   INTE-BARA-AUTO-KOD                                     
052203                MOVE LEV-IDDC           TO  WS-IDDC                       
052303                                                                          
052403              IF LEV-KDANMORS = '52' OR '53'  OR '72' OR                  
052503                 GOOD-DDC                     OR                          
052603                ((LEV-KDANMORS ='70' OR                                   
052703                                '92' OR '20'  OR '21') AND                
052803                (LEV-IDFTG    = '57' AND                                  
052903                 LEV-IDDC NOT = '61' AND '6A' AND '62'))                  
053003                IF LEV-KDVAT = SPACE AND (DIST79-DEALER-PRICE OR          
053203                                          DIST79-ECOM-PRICE)              
054000                  MOVE NEJ TO AUTO-KOD-SW                                 
055000                ELSE                                                      
056000                  MOVE LEV-IDDC TO WS-IDDC                                
057000                  IF NDC-PACIFIC                                          
058000                    CONTINUE                                              
059000                  ELSE                                                    
060000                    MOVE JA TO HAEMTA-USER-SW                             
070000                  END-IF                                                  
080000                END-IF                                                    
081000                                                                          
081100                IF LEV-KDKREBEH = 'Q  ' OR 'P  '                          
081200                  MOVE NEJ  TO AUTO-KOD-SW                                
081300                END-IF                                                    
081400              ELSE                                                        
081500                MOVE NEJ TO AUTO-KOD-SW                                   
081600              END-IF                                                      
081700                                                                          
081800              PERFORM IMS-GNP-WDA2-LEV                                    
081900            END-PERFORM                                                   
082000                                                                          
082100**-- KOLLA EVENTUELLA MID-RADER OM BARA AUTOKODER DÄR OCKSÅ.              
082200            IF BARA-AUTO-KOD                                              
082300              MOVE +1                    TO IX                            
082400              MOVE MID-KVRADER           TO MAX-IX                        
082500              PERFORM UNTIL IX > MAX-IX                                   
082600                IF MID-IDARTNR (IX) = SPACE                               
082700                  CONTINUE                                                
082800                ELSE                                                      
082900                  MOVE MID-IDDC(IX)      TO WS-IDDC                       
083000                  IF MID-KDANMORS (IX) = '52' OR '53' OR '72' OR          
084000                     GOOD-DDC                    OR                       
085000                    (MID-KDANMORS(IX) = ('70' OR                          
086000                                         '92' OR '20' OR '21') AND        
087000                    (MID-IDFTG(IX)    = '57' AND                          
087100                     MID-IDDC(IX) NOT = '61' AND '6A' AND '62'))          
087200                    IF MID-KDVAT (IX)  =                                  
087300                                 SPACE AND (DIST79-DEALER-PRICE OR        
087500                                            DIST79-ECOM-PRICE)            
087600                      MOVE NEJ TO AUTO-KOD-SW                             
087700                    END-IF                                                
087800                                                                          
087900                    IF MID-KDKREBEH (IX) = 'Q  ' OR 'P  '                 
088000                      MOVE NEJ  TO AUTO-KOD-SW                            
088100                    END-IF                                                
088200                  ELSE                                                    
088300                    MOVE NEJ TO AUTO-KOD-SW                               
088400                  END-IF                                                  
088500                END-IF                                                    
088600                ADD +1                TO IX                               
088700              END-PERFORM                                                 
088800            END-IF                                                        
088900                                                                          
089000            PERFORM IMS-GHU-WDA2-ANM                                      
089100            IF BARA-AUTO-KOD                                              
089200              MOVE '3'                   TO ANM-KDLEVANM                  
089300                                                                          
089400              MOVE ANM-IDFTG            TO WS-IDFTG                       
089500              IF (IDFTG-PV OR IDFTG-NON-VCC)                              
089600                  AND HAEMTA-USER-OK                                      
089700                PERFORM S05-HAEMTA-IDUSER-BEANST                          
089800              END-IF                                                      
089900            ELSE                                                          
090000              MOVE '1'                   TO ANM-KDLEVANM                  
090100            END-IF                                                        
090200            PERFORM IMS-REPL-WDA2                                         
090300          END-IF                                                          
090400       END-IF                                                             
090500     END-IF                                                               
090600                                                                          
090700     PERFORM IMS-GHU-WDA2-ANM                                             
090800     IF SEGMENT-FINNS                                                     
090900       MOVE ANM-KDVALISO TO WS-KDVALISO                                   
091000                                                                          
091100*-- FIX FÖR ATT EJ SKRIVA ÖVER STATUS 1 VID Q-RADER I NÄSTA BUNT.         
091200       IF MID-IDSYSTEM = 'REF' OR 'B72' OR 'FAK'                          
091300          IF MID-KDLEVANM = '1' AND ANM-KDLEVANM = '3'                    
091400            MOVE '1'   TO ANM-KDLEVANM                                    
091500            PERFORM IMS-REPL-WDA2                                         
091600          END-IF                                                          
091700       END-IF                                                             
091800       IF MID-IDSYSTEM = 'BAT' AND MID-KDANMORS(1) = '98'                 
091900          IF MID-KDLEVANM = '1' AND ANM-KDLEVANM = '3'                    
092000            MOVE '1'   TO ANM-KDLEVANM                                    
092100            PERFORM IMS-REPL-WDA2                                         
092200          END-IF                                                          
092300       END-IF                                                             
092400                                                                          
092500       PERFORM CB-LAEGG-UPP-NY-RAD                                        
092600     ELSE                                                                 
092700       PERFORM CA-LAEGG-UPP-NY-ROT                                        
092800       PERFORM CB-LAEGG-UPP-NY-RAD                                        
092900     END-IF                                                               
093000                                                                          
093100     IF ATTEST-KNOTA-KRAV                                                 
093200       PERFORM IMS-GHU-WDA2-ANM                                           
093300       IF SEGMENT-FINNS                                                   
093400         IF ANM-KDLEVATT = 0                                              
093500           MOVE 1 TO ANM-KDLEVATT                                         
093600           PERFORM IMS-REPL-WDA2                                          
093700         END-IF                                                           
093800       END-IF                                                             
093900     END-IF                                                               
094000                                                                          
094100     IF WS-IDDC-RET = SPACE                                               
094200       CONTINUE                                                           
094300     ELSE                                                                 
094400       PERFORM IMS-GHU-WDA2-ANM                                           
094500       IF SEGMENT-FINNS                                                   
094600         IF ANM-IDDC-RET = SPACE                                          
094700           MOVE WS-IDDC-RET  TO ANM-IDDC-RET                              
094800           MOVE WS-IXDCCLEAR TO ANM-IXDCCLEAR                             
094900           PERFORM IMS-REPL-WDA2                                          
095000         ELSE                                                             
095100           IF ANM-IXDCCLEAR = 3 OR 0                                      
095200             CONTINUE                                                     
095300           ELSE                                                           
095400             IF WS-IXDCCLEAR = 3                                          
095500               MOVE WS-IDDC-RET  TO ANM-IDDC-RET                          
095600               MOVE WS-IXDCCLEAR TO ANM-IXDCCLEAR                         
095700               PERFORM IMS-REPL-WDA2                                      
095800             ELSE                                                         
095900               IF ANM-IXDCCLEAR = 1 AND WS-IXDCCLEAR = 2                  
096000                 MOVE WS-IDDC-RET  TO ANM-IDDC-RET                        
096100                 MOVE WS-IXDCCLEAR TO ANM-IXDCCLEAR                       
096200                 PERFORM IMS-REPL-WDA2                                    
096300               END-IF                                                     
096400             END-IF                                                       
096500           END-IF                                                         
096600         END-IF                                                           
096700       END-IF                                                             
096800     END-IF                                                               
096900                                                                          
097000     IF UPPDAT-STATUS-2-OK                                                
097100*CHECK FOR "R" KDREDBEHK AND IF PRESENT THEN STATUS SHOULD BE 1           
097200          PERFORM IMS-GU-WDA2-ANM                                         
097300          IF SEGMENT-FINNS                                                
097400           PERFORM IMS-GNP-WDA2-LEV                                       
097500           PERFORM UNTIL SEGMENT-SAKNAS                                   
097600            IF LEV-KDKREBEH = 'R'                                         
097700               MOVE 'N'                 TO AUTO-APPROVAL-SW               
097800            END-IF                                                        
097900           PERFORM IMS-GNP-WDA2-LEV                                       
098000           END-PERFORM                                                    
098100          END-IF                                                          
098200          IF AUTO-APPROVAL-SW = 'Y'                                       
098300           PERFORM IMS-GHU-WDA2-ANM                                       
098400           IF SEGMENT-FINNS                                               
098500             IF ANM-KDLEVANM = '1' OR '3'                                 
098600                MOVE '2'                TO ANM-KDLEVANM                   
098700                PERFORM IMS-REPL-WDA2                                     
098800             END-IF                                                       
098900           END-IF                                                         
099000          END-IF                                                          
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400 CA-LAEGG-UPP-NY-ROT SECTION.                                             
099500                                                                          
099600     IF (MID-IDSYSTEM = 'LYNK' OR 'ECOM' OR 'VOUI' OR 'TAD '              
099700                               OR 'ACC ' OR 'APA'  OR 'APB'               
099800                               OR 'APC ' OR 'APD'  OR 'APE'               
099900                               OR 'APF ' OR 'APG'  OR 'APH'               
100000                               OR 'API ' OR 'APJ' )                       
100100                                                                          
100200       MOVE MID-IDSYSTEM                 TO ANM-IDSYSTEM                  
100300       MOVE '3'                          TO ANM-KDLEVANM                  
100400     END-IF                                                               
100500     MOVE MID-IDDISTR                    TO ANM-IDDISTR                   
100600     MOVE MID-IDKUNDNR                   TO ANM-IDKUNDNR                  
100700     MOVE MID-IDRAPPNR                   TO ANM-IDRAPPNR                  
100800     MOVE MID-IDFTG (1)                  TO ANM-IDFTG                     
100900     MOVE +0                             TO ANM-IDPERSON                  
101000     MOVE MSG-SIGNON-USERID              TO ANM-IDUSER                    
101100     MOVE SPACE                          TO ANM-KDARBTYP                  
101200     IF MID-IDSYSTEM = 'INT' OR 'REF' OR 'B72' OR 'FAK'                   
101300        MOVE MID-KDLEVANM                TO ANM-KDLEVANM                  
101400        IF MID-IDSYSTEM = 'INT'                                           
101500          MOVE '3'                       TO ANM-KDLEVANM                  
101600        END-IF                                                            
101700                                                                          
101800        MOVE ANM-IDFTG            TO WS-IDFTG                             
101900        IF IDFTG-PV OR IDFTG-NON-VCC                                      
102000          PERFORM S05-HAEMTA-IDUSER-BEANST                                
102100        END-IF                                                            
102200     ELSE                                                                 
102300        MOVE MID-KDLEVANM                TO ANM-KDLEVANM                  
102400        IF MID-KDLEVANM = '3'                                             
102500          PERFORM S05-HAEMTA-IDUSER-BEANST                                
102600        ELSE                                                              
102700          MOVE SPACE                     TO ANM-BEANST                    
102800          MOVE SPACE                     TO ANM-IDUSER-ADM                
102900        END-IF                                                            
103000     END-IF                                                               
103100     MOVE +0                             TO ANM-KVRADER-OBEH              
103200     MOVE +0                             TO ANM-KVRADER-RT                
103300                                                                          
103400     IF MID-PRFOERS  NOT = SPACE                                          
103500        MOVE MID-PRFOERS                 TO DEC-IDFRIDATA                 
103600        MOVE 7                           TO DEC-KVHELTAL                  
103700        MOVE 2                           TO DEC-KVDECIMAL                 
103800        CALL WDECEDIT USING WDECAREA                                      
103900        IF DEC-KDSVAR-OK                                                  
104000           MOVE DEC-IDEDITDATA           TO ANM-PRFOERS                   
104100        ELSE                                                              
104200           MOVE +0                       TO ANM-PRFOERS                   
104300        END-IF                                                            
104400     ELSE                                                                 
104500        MOVE +0                          TO ANM-PRFOERS                   
104600     END-IF                                                               
104700                                                                          
104800     IF MID-PRFRAKT  NOT = SPACE                                          
104900        MOVE MID-PRFRAKT                 TO DEC-IDFRIDATA                 
105000        MOVE 7                           TO DEC-KVHELTAL                  
105100        MOVE 2                           TO DEC-KVDECIMAL                 
105200        CALL WDECEDIT USING WDECAREA                                      
105300        IF DEC-KDSVAR-OK                                                  
105400           MOVE DEC-IDEDITDATA           TO ANM-PRFRAKT                   
105500        ELSE                                                              
105600           MOVE +0                       TO ANM-PRFRAKT                   
105700        END-IF                                                            
105800     ELSE                                                                 
105900        MOVE +0                          TO ANM-PRFRAKT                   
106000     END-IF                                                               
106100                                                                          
106200     IF MID-PRLEGKST NOT = SPACE                                          
106300        MOVE MID-PRLEGKST                TO DEC-IDFRIDATA                 
106400        MOVE 7                           TO DEC-KVHELTAL                  
106500        MOVE 2                           TO DEC-KVDECIMAL                 
106600        CALL WDECEDIT USING WDECAREA                                      
106700        IF DEC-KDSVAR-OK                                                  
106800           MOVE DEC-IDEDITDATA           TO ANM-PRLEGKST                  
106900        ELSE                                                              
107000           MOVE +0                       TO ANM-PRLEGKST                  
107100        END-IF                                                            
107200     ELSE                                                                 
107300        MOVE +0                          TO ANM-PRLEGKST                  
107400     END-IF                                                               
107500                                                                          
107600     MOVE +0                             TO ANM-REEMBHNT                  
107700     MOVE MID-IDFTG (1)                  TO WS-IDFTG                      
107800     IF IDFTG-PV                                                          
107900        IF MID-RELANDCO NOT = SPACE                                       
108000           MOVE MID-RELANDCO             TO DEC-IDFRIDATA                 
108100           MOVE 3                        TO DEC-KVHELTAL                  
108200           MOVE 2                        TO DEC-KVDECIMAL                 
108300           CALL WDECEDIT USING WDECAREA                                   
108400           IF DEC-KDSVAR-OK                                               
108500              MOVE DEC-IDEDITDATA        TO ANM-RELANDCO                  
108600           ELSE                                                           
108700              MOVE +0                    TO ANM-RELANDCO                  
108800           END-IF                                                         
108900        ELSE                                                              
109000          IF DIST79-DEALER-PRICE OR                                       
109200             DIST79-ECOM-PRICE                                            
109300              MOVE +0                     TO ANM-RELANDCO                 
109400          ELSE                                                            
109500            PERFORM IMS-GET-WDB201                                        
109600                                                                          
109700            IF SEGMENT-FINNS                                              
109800              MOVE GMT-IDPARTNR           TO W-IDPARTNR                   
109900              MOVE GMT-IDFTG              TO W-IDFTG                      
110000                                                                          
110100              PERFORM IMS-GET-WDB101                                      
110200              IF SEGMENT-FINNS                                            
110300                 MOVE BET-RELANDCO        TO ANM-RELANDCO                 
110400              ELSE                                                        
110500                 MOVE ZERO                TO ANM-RELANDCO                 
110600              END-IF                                                      
110700            ELSE                                                          
110800               MOVE +0                    TO ANM-RELANDCO                 
110900            END-IF                                                        
111000          END-IF                                                          
111100        END-IF                                                            
111200     ELSE                                                                 
111300        MOVE +0                           TO ANM-RELANDCO                 
111400     END-IF                                                               
111500                                                                          
111600     MOVE MID-TILEVANM                   TO ANM-DALEVANM                  
111700     IF MID-TILEVANM NOT = ZERO                                           
111800       IF MID-TILEVANM < 500000                                           
111900         MOVE 20                         TO ANM-DALEVANM (1:2)            
112000       ELSE                                                               
112100         IF MID-TILEVANM < 999999                                         
112200           MOVE 19                       TO ANM-DALEVANM (1:2)            
112300         ELSE                                                             
112400           MOVE 99999999                 TO ANM-DALEVANM                  
112500         END-IF                                                           
112600       END-IF                                                             
112700     END-IF                                                               
112800     MOVE +0                             TO ANM-DARETANK                  
112900     MOVE +0                             TO ANM-DARETILL                  
113000     MOVE NEJ                            TO ANM-FLFARLIG                  
113100     MOVE +0                             TO ANM-DARTPMN                   
113200     MOVE SPACE                          TO ANM-KDLEVANM-UPD              
113300     MOVE +0                             TO ANM-KDLEVATT                  
113400     MOVE SPACE                          TO ANM-IDDC-RET                  
113500     MOVE 0                              TO ANM-IXDCCLEAR                 
113600                                                                          
113700     IF MID-KDVALISO = SPACE                                              
113800       IF DIST79-DEALER-PRICE                                             
114000         PERFORM IMS-GET-WDB201                                           
114100                                                                          
114200         IF SEGMENT-FINNS                                                 
114300           MOVE GMT-IDPARTNR             TO W-IDPARTNR                    
114400           MOVE GMT-IDFTG                TO W-IDFTG                       
114500                                                                          
114600           PERFORM IMS-GET-WDB101                                         
114700           IF SEGMENT-FINNS                                               
114800             MOVE BET-KDVALISO           TO ANM-KDVALISO                  
114900                                            WS-KDVALISO                   
115000           ELSE                                                           
115100             MOVE 'SEK'                  TO ANM-KDVALISO                  
115200                                            WS-KDVALISO                   
115300           END-IF                                                         
115400         ELSE                                                             
115500           MOVE SPACE                    TO ANM-KDVALISO                  
115600                                            WS-KDVALISO                   
115700         END-IF                                                           
115800       ELSE                                                               
115900         IF DIST79-ECOM-PRICE                                             
116000           MOVE SPACE                    TO ANM-KDVALISO                  
116100                                            WS-KDVALISO                   
116200         ELSE                                                             
116300           MOVE 'SEK'                    TO ANM-KDVALISO                  
116400                                            WS-KDVALISO                   
116500         END-IF                                                           
116600       END-IF                                                             
116700     ELSE                                                                 
116800       MOVE MID-KDVALISO                 TO ANM-KDVALISO                  
116900                                            WS-KDVALISO                   
117000     END-IF                                                               
117100                                                                          
117200     PERFORM IMS-ISRT-WDA2-ROT                                            
117300                                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 CB-LAEGG-UPP-NY-RAD SECTION.                                             
117700                                                                          
117800     MOVE +1                             TO IX                            
117900     MOVE MID-KVRADER                    TO MAX-IX                        
118000                                                                          
118100     PERFORM IMS-GU-WDR501-4103-GE                                        
118200     IF SEGMENT-SAKNAS                                                    
118300       MOVE NEJ TO R5-UPD-SW                                              
118400     ELSE                                                                 
118500       MOVE JA  TO R5-UPD-SW                                              
118600     END-IF                                                               
118700                                                                          
118800     PERFORM IMS-GET-WDB201                                               
118900     IF SEGMENT-FINNS                                                     
119000       MOVE JA  TO WDB201-SW                                              
119100     ELSE                                                                 
119200       MOVE NEJ TO WDB201-SW                                              
119300     END-IF                                                               
119400                                                                          
119500     PERFORM UNTIL IX > MAX-IX                                            
119600        IF MID-IDARTNR (IX) = SPACE                                       
119700           CONTINUE                                                       
119800        ELSE                                                              
119900           MOVE MID-IDARTNR    (IX)      TO LEV-IDARTNR                   
120000           IF (MID-IDSYSTEM = 'STA'  OR 'INT'  OR 'LYNK' OR               
120100                              'ECOM' OR 'VOUI' OR 'TAD ' OR               
120200                              'ACC ' OR 'APA'  OR 'APB'  OR               
120300                              'APC ' OR 'APD'  OR 'APE'  OR               
120400                              'APF ' OR 'APG'  OR 'APH'  OR               
120500                              'API ' OR 'APJ' )                           
120600              MOVE +1                    TO LEV-IDRADNR                   
120700           ELSE                                                           
120800              MOVE MID-IDRADNR (IX)      TO WS-IDRADNR                    
120900              IF MID-KDANMORS (IX) = ('96' OR '98') AND                   
121000                 WS-IDRADNR = ZERO                                        
121100                 MOVE +1                 TO LEV-IDRADNR                   
121200              ELSE                                                        
121300                 MOVE WS-IDRADNR         TO LEV-IDRADNR                   
121400              END-IF                                                      
121500           END-IF                                                         
121600           MOVE +0                       TO LEV-ADGANG                    
121700           MOVE +0                       TO LEV-ADLAGOMR                  
121800           MOVE +0                       TO LEV-ADPLATS                   
121900           MOVE MID-FLANLYSF   (IX)      TO LEV-FLANLYSF                  
122000           MOVE NEJ                      TO LEV-FLANNULL                  
122100           MOVE MID-FLAUTKRE   (IX)      TO LEV-FLAUTKRE                  
122200           MOVE MID-FLDIRLEV   (IX)      TO LEV-FLDIRLEV                  
122300           MOVE NEJ                      TO LEV-FLSKROT                   
122400           MOVE SPACE                    TO LEV-FLSVAR                    
122500           MOVE SPACE                    TO LEV-FLRETUR                   
122600           MOVE NEJ                      TO LEV-FLTEXT                    
122700           MOVE MID-IDANALYS   (IX)      TO LEV-IDANALYS                  
122800           MOVE MID-IDKONTO    (IX)      TO LEV-IDKONTO                   
122900           MOVE MID-IDKST      (IX)      TO LEV-IDKST                     
123000           MOVE +0                       TO LEV-IDANSTNR-RET              
123100           MOVE MID-IDDC       (IX)      TO LEV-IDDC                      
123200           MOVE MID-IDDC-RET   (IX)      TO LEV-IDDC-RET                  
123300                                                                          
123400*** FÖR RETURER MÅSTE MAN KOLLA IDDC-RET MOT WDA201.                      
123500           MOVE MID-KDANMORS   (IX)      TO OKOD-KDANMORS                 
123600           CALL W418OKOD USING OKOD-W418OKOD                              
123700           IF OKOD-FL-RETILL = JA  AND WDB201-FINNS                       
123800                                                                          
123900             IF WS-IXDCCLEAR = 3                                          
124000               CONTINUE                                                   
124100             ELSE                                                         
124200               IF GMT-FLLDCKND = JA OR                                    
124300                  GMT-FLRETUR  = JA                                       
124400                 IF MID-IDDC-RET (IX) = GMT-IDDC-RET72 (1)                
124500                   IF WS-IXDCCLEAR = 0                                    
124600                     MOVE MID-IDDC-RET (IX) TO WS-IDDC-RET                
124700                     MOVE 1                 TO WS-IXDCCLEAR               
124800                   END-IF                                                 
124900                 ELSE                                                     
125000                   IF MID-IDDC-RET (IX) = GMT-IDDC-RET72 (2)              
125100                     IF WS-IXDCCLEAR = 1 OR 0                             
125200                       MOVE MID-IDDC-RET (IX) TO WS-IDDC-RET              
125300                       MOVE 2                 TO WS-IXDCCLEAR             
125400                     END-IF                                               
125500                   ELSE                                                   
125600                     IF MID-IDDC-RET (IX) = GMT-IDDC-RET72 (3)            
125700                       MOVE MID-IDDC-RET (IX) TO WS-IDDC-RET              
125800                       MOVE 3                 TO WS-IXDCCLEAR             
125900                     ELSE                                                 
126000                       MOVE MID-IDDC-RET (IX) TO WS-IDDC-RET              
126100                       MOVE 3                 TO WS-IXDCCLEAR             
126200                     END-IF                                               
126300                   END-IF                                                 
126400                 END-IF                                                   
126500               ELSE                                                       
126600                 MOVE MID-IDDC-RET (IX)    TO WS-IDDC-RET                 
126700                 MOVE 3                    TO WS-IXDCCLEAR                
126800               END-IF                                                     
126900             END-IF                                                       
127000           ELSE                                                           
127100             IF OKOD-FL-INTERNUPPACKNING = JA                             
127200               MOVE MID-IDDC-RET  (IX)   TO WS-IDDC-RET                   
127300               MOVE 0                    TO WS-IXDCCLEAR                  
127400             END-IF                                                       
127500           END-IF                                                         
127600                                                                          
127700           MOVE MID-IDFAKT     (IX)      TO LEV-IDFAKT                    
127800           MOVE MID-IDFAKT-LOC (IX)      TO LEV-IDFAKT-LOC                
127900           MOVE MID-IDFTG      (IX)      TO LEV-IDFTG                     
128000           MOVE ZERO                     TO LEV-IDILIST                   
128100           MOVE +0                       TO LEV-IDKNOTNR                  
128200           MOVE MID-IDKOLLI    (IX)      TO LEV-IDKOLLI                   
128300           MOVE MID-IDKUNDRF   (IX)      TO LEV-IDKUNDRF                  
128400           IF MID-IDLOPNRM     (IX) NUMERIC                               
128500             MOVE MID-IDLOPNRM (IX)        TO LEV-IDLOPNRM                
128600           ELSE                                                           
128700             MOVE +0                       TO LEV-IDLOPNRM                
128800           END-IF                                                         
128900           MOVE +0                       TO LEV-IDPERSON-REM              
129000           MOVE MID-IDUSER-PACK  (IX)    TO LEV-IDUSER-PACK               
129100           MOVE MID-KDANMORS   (IX)      TO LEV-KDANMORS                  
129200                                                                          
129300                                                                          
129400                                                                          
129500           MOVE SPACE                    TO LEV-KDARBTYP-REM              
129600           MOVE MID-KDEMBLEV   (IX)      TO LEV-KDEMBLEV                  
129700           MOVE MID-KDFAKTYP   (IX)      TO LEV-KDFAKTYP                  
129800           MOVE SPACE                    TO LEV-KDFAKTYP-KNOT             
129900           MOVE MID-KDFRAKT    (IX)      TO LEV-KDFRAKT                   
130000           IF MID-IDSYSTEM = 'REF'                                        
130100             IF MID-KDKREBEH (IX) = 'Q  ' OR 'P  '                        
130200               CONTINUE                                                   
130300             ELSE                                                         
130400               MOVE 'Y  '                TO LEV-KDKREBEH                  
130500             END-IF                                                       
130600           ELSE                                                           
130700              MOVE MID-KDKREBEH (IX)     TO LEV-KDKREBEH                  
130800           END-IF                                                         
130900           MOVE MID-KDORDKL      (IX)    TO LEV-KDORDKL                   
131000           MOVE +0                       TO LEV-KVANTAL-ILI               
131100           MOVE +0                       TO LEV-KVAVV-KVAL                
131200           MOVE +0                       TO LEV-KVAVV-KVANT               
131300           MOVE MID-KVLEVANM   (IX)      TO LEV-KVLEVANM                  
131400                                            LEV-KVLEVANM-BEKR             
131500           MOVE +0                       TO LEV-KVRETINL                  
131600           MOVE +0                       TO LEV-KVRETINL-SKR              
131700                                                                          
131800           IF MID-PRARTBTO (IX) NOT = SPACE                               
131900              MOVE MID-PRARTBTO (IX)     TO DEC-IDFRIDATA                 
132000              MOVE 7                     TO DEC-KVHELTAL                  
132100              MOVE 2                     TO DEC-KVDECIMAL                 
132200              CALL WDECEDIT USING WDECAREA                                
132300              IF DEC-KDSVAR-OK                                            
132400                 MOVE DEC-IDEDITDATA     TO LEV-PRARTBTO                  
132500              ELSE                                                        
132600                 MOVE +0                 TO LEV-PRARTBTO                  
132700              END-IF                                                      
132800           ELSE                                                           
132900              MOVE +0                    TO LEV-PRARTBTO                  
133000           END-IF                                                         
133100                                                                          
133200           IF MID-PRARTBTO-LOC (IX) NOT = SPACE                           
133300              MOVE MID-PRARTBTO-LOC(IX)  TO DEC-IDFRIDATA                 
133400              MOVE 7                     TO DEC-KVHELTAL                  
133500              MOVE 2                     TO DEC-KVDECIMAL                 
133600              CALL WDECEDIT USING WDECAREA                                
133700              IF DEC-KDSVAR-OK                                            
133800                 MOVE DEC-IDEDITDATA     TO LEV-PRARTBTO-LOC              
133900              ELSE                                                        
134000                 MOVE +0                 TO LEV-PRARTBTO-LOC              
134100              END-IF                                                      
134200           ELSE                                                           
134300              MOVE +0                    TO LEV-PRARTBTO-LOC              
134400           END-IF                                                         
134500                                                                          
134600           IF MID-PRARTBTO-LOCINV (IX) NOT = SPACE                        
134700              MOVE MID-PRARTBTO-LOCINV(IX)  TO DEC-IDFRIDATA              
134800              MOVE 7                     TO DEC-KVHELTAL                  
134900              MOVE 2                     TO DEC-KVDECIMAL                 
135000              CALL WDECEDIT USING WDECAREA                                
135100              IF DEC-KDSVAR-OK                                            
135200                 MOVE DEC-IDEDITDATA     TO LEV-PRARTBTO-LOCINV           
135300              ELSE                                                        
135400                 MOVE +0                 TO LEV-PRARTBTO-LOCINV           
135500              END-IF                                                      
135600           ELSE                                                           
135700              MOVE +0                    TO LEV-PRARTBTO-LOCINV           
135800           END-IF                                                         
135900                                                                          
136000           IF MID-PRFRAKT-RAD (IX) NOT = SPACE                            
136100              MOVE MID-PRFRAKT-RAD(IX)   TO DEC-IDFRIDATA                 
136200              MOVE 7                     TO DEC-KVHELTAL                  
136300              MOVE 2                     TO DEC-KVDECIMAL                 
136400              CALL WDECEDIT USING WDECAREA                                
136500              IF DEC-KDSVAR-OK                                            
136600                 MOVE DEC-IDEDITDATA     TO LEV-PRFRAKT                   
136700              ELSE                                                        
136800                 MOVE +0                 TO LEV-PRFRAKT                   
136900              END-IF                                                      
137000           ELSE                                                           
137100              MOVE +0                    TO LEV-PRFRAKT                   
137200           END-IF                                                         
137300                                                                          
137400           IF MID-TIFAKT (IX) NOT = SPACE                                 
137500              MOVE MID-TIFAKT (IX)       TO LEV-TIFAKT                    
137600           ELSE                                                           
137700              MOVE +0                    TO LEV-TIFAKT                    
137800           END-IF                                                         
137900                                                                          
138000           IF MID-TIFAKT-LOC (IX) NOT = SPACE                             
138100              MOVE MID-TIFAKT-LOC(IX)    TO LEV-TIFAKT-LOC                
138200           ELSE                                                           
138300              MOVE +0                    TO LEV-TIFAKT-LOC                
138400           END-IF                                                         
138500                                                                          
138600           MOVE +0                       TO LEV-TIINLINL                  
138700           MOVE +0                       TO LEV-TIKNOTA                   
138800           MOVE MID-TILEVANM-RAD (IX)    TO LEV-DALEVANM                  
138900           IF MID-TILEVANM-RAD (IX) NOT = ZERO                            
139000             IF MID-TILEVANM-RAD (IX) < 500000                            
139100               MOVE 20                   TO LEV-DALEVANM (1:2)            
139200             ELSE                                                         
139300               IF MID-TILEVANM-RAD (IX) < 999999                          
139400                 MOVE 19                 TO LEV-DALEVANM (1:2)            
139500               ELSE                                                       
139600                 MOVE 99999999           TO LEV-DALEVANM                  
139700               END-IF                                                     
139800             END-IF                                                       
139900           END-IF                                                         
140000           MOVE +0                       TO LEV-TIREMISS-IN               
140100           MOVE +0                       TO LEV-TIREMISS-UT               
140200           MOVE +0                       TO LEV-TIUTSKR                   
140300           MOVE +0                       TO LEV-IDARTNR-DEL               
140400           MOVE +0                       TO LEV-TIUPPDAT-ILI              
140500           MOVE SPACE                    TO LEV-FLLSBOK                   
140600           MOVE +0                       TO LEV-KDAVVTYP                  
140700           MOVE SPACE                    TO LEV-FLINVUPD                  
140800           MOVE MID-FLPRQUES (IX)        TO LEV-FLPRQUES                  
140900           MOVE MID-KDVAT (IX)           TO LEV-KDVAT                     
141000           MOVE MID-BEART-VIPS (IX)      TO LEV-BEART-VIPS                
141100           MOVE ZERO                     TO LEV-IDPRQUES                  
141200           MOVE +0                       TO LEV-IDANSTNR-ILIU             
141300                                                                          
141400           IF DIST79-DEALER-PRICE                                         
141500             MOVE NEJ      TO HAEMTA-PRIS-SW                              
141600                                                                          
141700             IF MID-KDANMORS (IX) = '12' OR '13' OR '22' OR               
141800                                    '23' OR '84' OR '99' OR               
141900                                    '27' OR '28'                          
142000                                                                          
142100               CONTINUE                                                   
142200             ELSE                                                         
142300               IF MID-KDVAT (IX) = SPACE                                  
142400                 MOVE JA   TO  HAEMTA-PRIS-SW                             
142500                 MOVE JA                   TO LEV-FLPRQUES                
142600               END-IF                                                     
142700                                                                          
142800               IF MID-IDSYSTEM = 'STA'                                    
142900                 IF MID-BEART-VIPS (IX) = SPACE                           
143000                   MOVE JA   TO  HAEMTA-PRIS-SW                           
143100                 END-IF                                                   
143200               END-IF                                                     
143300             END-IF                                                       
143400                                                                          
143500             IF MID-FLPRQUES (IX) = JA                                    
143600               MOVE JA   TO  HAEMTA-PRIS-SW                               
143700             END-IF                                                       
143800                                                                          
143900             IF HAEMTA-PRIS-VIPS                                          
144000               IF WS-IDPRQUES = ZERO                                      
144100                 PERFORM CBA-HAEMTA-FRAGENR-W335PRNO                      
144200               ELSE                                                       
144300                 PERFORM CBB-ADD-1-TO-FRAGENR-W335PRNO                    
144400               END-IF                                                     
144500               MOVE WS-IDPRQUES          TO LEV-IDPRQUES                  
144600             END-IF                                                       
144700                                                                          
144800           END-IF                                                         
144900                                                                          
145000           IF MID-PRARTSTD (IX) NOT = SPACE                               
145100              MOVE MID-PRARTSTD (IX)     TO DEC-IDFRIDATA                 
145200              MOVE 7                     TO DEC-KVHELTAL                  
145300              MOVE 2                     TO DEC-KVDECIMAL                 
145400              CALL WDECEDIT USING WDECAREA                                
145500              IF DEC-KDSVAR-OK                                            
145600                 MOVE DEC-IDEDITDATA     TO LEV-PRARTSTD                  
145700              ELSE                                                        
145800                 MOVE +0                 TO LEV-PRARTSTD                  
145900              END-IF                                                      
146000           ELSE                                                           
146100              MOVE +0                    TO LEV-PRARTSTD                  
146200           END-IF                                                         
146300                                                                          
146400* FOR NON-VCC DEALER WE PUT AVG COST FROM VIPS IN LOCAL CURRNECY          
146500           IF MID-PRARTSJK (IX) NOT = SPACE                               
146600              MOVE MID-PRARTSJK (IX)     TO DEC-IDFRIDATA                 
146700              MOVE 7                     TO DEC-KVHELTAL                  
146800              MOVE 2                     TO DEC-KVDECIMAL                 
146900              CALL WDECEDIT USING WDECAREA                                
147000              IF DEC-KDSVAR-OK                                            
147100                 MOVE DEC-IDEDITDATA     TO LEV-PRARTSJK                  
147200              ELSE                                                        
147300                 MOVE +0                 TO LEV-PRARTSJK                  
147400              END-IF                                                      
147500           ELSE                                                           
147600              MOVE +0                    TO LEV-PRARTSJK                  
147700           END-IF                                                         
147800                                                                          
147900*** KOLLA IFALL KODEN GER KNOTA SOM SKALL ATTESTERAS.                     
148000*** SKALL EJ GÄLLA NA (FTG=53/54) ELLER JAP/AU DC=61/62 ÄNNU.             
148100*** SKALL EJ GÄLLA IN (FTG=61).                                           
148200           MOVE LEV-IDDC TO WS-IDDC                                       
148300           MOVE MID-IDFTG (IX) TO WS-IDFTG                                
148400           IF NDC-PACIFIC OR IDFTG-US OR IDFTG-CA OR NDC-NX OR            
148500              NDC-NS                                                      
148600             CONTINUE                                                     
148700           ELSE                                                           
148800             IF MID-KDANMORS (IX) = '12' OR '22' OR '99' OR               
148900                                    '13' OR '23' OR '84' OR               
149000                                    '27' OR '28' OR '74'                  
149100               CONTINUE                                                   
149200             ELSE                                                         
149300               MOVE MID-KDANMORS   (IX)      TO OKOD-KDANMORS             
149400               CALL W418OKOD USING OKOD-W418OKOD                          
149500                                                                          
149600               IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                        
149700                  (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                    
149800                  (OKOD-FL-KRENOT-EFTER-RT = JA) OR                       
149900                  MID-KDANMORS (IX) = '97'                                
150000                                                                          
150100                 MOVE JA TO ATTEST-KNOTA-SW                               
150200                 IF (OKOD-FL-KRENOT-EFTER-RT = JA) OR                     
150300                   MID-KDANMORS (IX) = '97'                               
150400                   MOVE 'RP' TO WS-KDKRENOT                               
150500                 ELSE                                                     
150600                   MOVE 'CN' TO WS-KDKRENOT                               
150700                 END-IF                                                   
150800                 IF WDR501-SAKNAS                                         
150900                    MOVE W-WDGXKEY-4103-X TO 4103-WDGX4103                
151000                    PERFORM IMS-ISRT-WDR501-4103                          
151100                    PERFORM IMS-GU-WDR501-4103                            
151200                    MOVE JA  TO R5-UPD-SW                                 
151300                    PERFORM CBE-ISRT-OR-REPL-4104                         
151400                 ELSE                                                     
151500                    PERFORM CBE-ISRT-OR-REPL-4104                         
151600                 END-IF                                                   
151700               END-IF                                                     
151800             END-IF                                                       
151900           END-IF                                                         
152000                                                                          
152100           PERFORM BD-ANROPA-W418ANSV                                     
152200                                                                          
152300           PERFORM IMS-ISRT-WDA2-RAD                                      
152400           IF SEGMENT-REDAN-FINNS                                         
152500              PERFORM UNTIL SEGMENT-FINNS                                 
152600                ADD +1                   TO LEV-IDRADNR                   
152700                PERFORM IMS-ISRT-WDA2-RAD                                 
152800              END-PERFORM                                                 
152900           END-IF                                                         
153000                                                                          
153100           IF HAEMTA-PRIS-VIPS                                            
153200             PERFORM IMS-GU-WDC701                                        
153300             IF SEGMENT-FINNS                                             
153400                PERFORM CBD-UPPDATERA-PRISFRAGA-RAD                       
153500             ELSE                                                         
153600                PERFORM CBC-UPPDATERA-PRISFRAGA-ROT                       
153700                PERFORM CBD-UPPDATERA-PRISFRAGA-RAD                       
153800             END-IF                                                       
153900             PERFORM S01-SKICKA-PRISFRAGA-W30391                          
154000           END-IF                                                         
154100        END-IF                                                            
154200        ADD +1                TO IX                                       
154300     END-PERFORM                                                          
154400     .                                                                    
154500     EJECT                                                                
154600 CBA-HAEMTA-FRAGENR-W335PRNO SECTION.                                     
154700                                                                          
154800     MOVE +1                TO PRNO-KDCALL                                
154900     MOVE ZERO              TO PRNO-IDPRQUES-IN                           
155000     CALL W335PRNO USING PRNO-W335PRNO PRNO-PCB                           
155100     MOVE PRNO-IDPRQUES-UT  TO WS-IDPRQUES                                
155200                                                                          
155300     .                                                                    
155400     EJECT                                                                
155500 CBB-ADD-1-TO-FRAGENR-W335PRNO SECTION.                                   
155600                                                                          
155700     MOVE +2                TO PRNO-KDCALL                                
155800     MOVE WS-IDPRQUES       TO PRNO-IDPRQUES-IN                           
155900     CALL W335PRNO USING PRNO-W335PRNO PRNO-PCB                           
156000     MOVE PRNO-IDPRQUES-UT  TO WS-IDPRQUES                                
156100     .                                                                    
156200     EJECT                                                                
156300 CBC-UPPDATERA-PRISFRAGA-ROT SECTION.                                     
156400                                                                          
156500     MOVE MID-IDDISTR       TO PRQ-IDDISTR                                
156600     MOVE MID-IDKUNDNR      TO PRQ-IDKUNDNR                               
156700     MOVE SPACE             TO PRQ-IDBUNDLE                               
156800     MOVE MID-IDRAPPNR      TO PRQ-IDRAPPNR                               
156900                                                                          
157000     PERFORM IMS-ISRT-WDC701                                              
157100                                                                          
157200     .                                                                    
157300     EJECT                                                                
157400 CBD-UPPDATERA-PRISFRAGA-RAD SECTION.                                     
157500                                                                          
157600     MOVE LEV-IDPRQUES      TO LPRQ-IDPRQUES                              
157700     MOVE 'CARPARTS.PULS.CREPRICE'   TO LPRQ-ADDISPABS                    
157800     MOVE SPACE             TO LPRQ-KDPRSTA                               
157900     IF DIST03-HELA-NORDEN                                                
158000       MOVE +4              TO LPRQ-KDORDKL                               
158100     ELSE                                                                 
158200       MOVE +3              TO LPRQ-KDORDKL                               
158300     END-IF                                                               
158400     MOVE MID-IDARTNR (IX)  TO LPRQ-IDARTNR                               
158500     MOVE MID-KVLEVANM (IX) TO LPRQ-KVBEART                               
158600     MOVE NEJ               TO LPRQ-FLARTSTD                              
158700     MOVE SPACE             TO LPRQ-KDORDTYP                              
158800     MOVE ZERO              TO LPRQ-PRARTBTO-LOC                          
158900     MOVE ZERO              TO LPRQ-PRARTNTO-LOC                          
159000     MOVE SPACE             TO LPRQ-KDRAB                                 
159100     MOVE SPACE             TO LPRQ-KDVALISO                              
159200     MOVE SPACE             TO LPRQ-KDVAT                                 
159300     MOVE ZERO              TO LPRQ-REARTRAB                              
159400     MOVE SPACE             TO LPRQ-BEART-VIPS                            
159500     MOVE NEJ               TO LPRQ-FLALL                                 
159600     MOVE ZERO              TO LPRQ-KVANTAL-AVBOK                         
159700                                                                          
159800     MOVE FUNCTION CURRENT-DATE (1:14) TO LPRQ-DADATTID-REG               
159900                                                                          
160000     MOVE ZERO              TO LPRQ-DADATTID-SEND                         
160100     MOVE ZERO              TO LPRQ-DADATTID-SVAR                         
160200     MOVE ZERO              TO LPRQ-DADATTID-OK                           
160300     MOVE ZERO              TO LPRQ-KDFEL                                 
160400     MOVE ZERO              TO LPRQ-KDSKEPP                               
160500     MOVE SPACE             TO LPRQ-FILLER1                               
160600     MOVE SPACE             TO LPRQ-KDSEGKEY                              
160700     MOVE SPACE             TO LPRQ-FILLER                                
160800                                                                          
160900     PERFORM IMS-ISRT-WDC711                                              
161000                                                                          
161100     .                                                                    
161200     EJECT                                                                
161300 CBE-ISRT-OR-REPL-4104 SECTION.                                           
161400                                                                          
161500     MOVE WS-KDKRENOT   TO W-KDKRENOT-4104                                
161600     MOVE MID-IDDC (IX) TO W-IDDC-4104                                    
161700                                                                          
161800     PERFORM IMS-GHNP-WDGX4104                                            
161900                                                                          
162000*CHINA-PRICE                                                              
162100*INDIA-PRICE                                                              
162200*KOREA-PRICE                                                              
162300*TURKEY-PRICE                                                             
162400*MEXICO-PRICE                                                             
162500*BRASIL-PRICE                                                             
162600     IF IDFTG-CN OR IDFTG-IN OR IDFTG-KR OR IDFTG-TR OR IDFTG-MY          
162700     OR IDFTG-TH OR IDFTG-TW OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA          
162800       IF LEV-PRARTBTO-LOCINV > ZERO                                      
162900                                                                          
163000         COMPUTE WS-SUKRENOT ROUNDED =                                    
163100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
163200       END-IF                                                             
163300     ELSE                                                                 
163400       IF LEV-PRARTBTO > ZERO                                             
163500         COMPUTE WS-PRARTBTO = LEV-KVLEVANM-BEKR * LEV-PRARTBTO           
163600         COMPUTE WS-SUKRENOT = WS-SUKRENOT + WS-PRARTBTO                  
163700       ELSE                                                               
163800         IF LEV-PRARTBTO-LOC > ZERO                                       
163900           COMPUTE WS-PRARTBTO-LOC = LEV-KVLEVANM-BEKR *                  
164000                                     LEV-PRARTBTO-LOC                     
164100           COMPUTE WS-SUKRENOT = WS-SUKRENOT + WS-PRARTBTO-LOC            
164200         END-IF                                                           
164300       END-IF                                                             
164400     END-IF                                                               
164500                                                                          
164600     IF SEGMENT-SAKNAS                                                    
164700       MOVE MID-IDDC (IX)                  TO 4104-IDDC                   
164800       MOVE WS-KDKRENOT                    TO 4104-KDKRENOT               
164900       MOVE NEJ                            TO 4104-FLKREATT               
165000       EVALUATE TRUE                                                      
165100       WHEN IDFTG-CN                                                      
165200           MOVE 'CNY'                      TO 4104-KDVALISO               
165300       WHEN IDFTG-IN                                                      
165400           MOVE 'INR'                      TO 4104-KDVALISO               
165500       WHEN IDFTG-KR                                                      
165600           MOVE 'KRW'                      TO 4104-KDVALISO               
165700       WHEN IDFTG-TR                                                      
165800           MOVE 'TRY'                      TO 4104-KDVALISO               
165900       WHEN IDFTG-MX                                                      
166000           MOVE 'MXN'                      TO 4104-KDVALISO               
166100       WHEN IDFTG-BR                                                      
166200           MOVE 'BRL'                      TO 4104-KDVALISO               
166300       WHEN IDFTG-MY                                                      
166400           MOVE 'MYR'                      TO 4104-KDVALISO               
166500       WHEN IDFTG-TH                                                      
166600           MOVE 'THB'                      TO 4104-KDVALISO               
166700       WHEN IDFTG-TW                                                      
166800           MOVE 'TWD'                      TO 4104-KDVALISO               
166810       WHEN IDFTG-ZA                                                      
166820           MOVE 'ZAR'                      TO 4104-KDVALISO               
166900       WHEN OTHER                                                         
167000           MOVE WS-KDVALISO                TO 4104-KDVALISO               
167100       END-EVALUATE                                                       
167200       MOVE SPACE                          TO 4104-FILLER                 
167300       MOVE WS-SUKRENOT                    TO 4104-SUKRENOT               
167400       PERFORM IMS-ISRT-WDGX4104                                          
167500     ELSE                                                                 
167600       ADD WS-SUKRENOT                     TO 4104-SUKRENOT               
167700       PERFORM IMS-REPL-WDGX4104                                          
167800     END-IF                                                               
167900                                                                          
168000     MOVE ZERO                             TO WS-SUKRENOT                 
168100     MOVE SPACE                            TO WS-KDKRENOT                 
168200     .                                                                    
168300     EJECT                                                                
168400 BD-ANROPA-W418ANSV SECTION.                                              
168500                                                                          
168600     MOVE MID-IDFTG (IX)                 TO WS-IDFTG                      
168700     MOVE LEV-IDDC                       TO WS-IDDC                       
168800     IF IDFTG-PV                                                          
168900       IF NOT NDC                                                         
169000         PERFORM BDA-KOLLA-OM-AUTO-REM                                    
169100       END-IF                                                             
169200     END-IF                                                               
169300     PERFORM IMS-GET-WDB201                                               
169400     IF SEGMENT-FINNS                                                     
169500       MOVE GMT-KDKUNDKAT          TO W-KDKUNDKAT                         
169600     END-IF                                                               
169700                                                                          
169800* IF REFILL DISTRICT THEN DO NOT DO AUTO REMISS AS IT GET END  UP         
169900* IN REM088 IN 4721  TO AVOID IT ADDED A CONTROL TO CHECK CUST-CAT        
170000* FOR REFILL IT IS 08 HENCE SKIPPING IT                                   
170100     IF AUTO-REM-OK AND W-KDKUNDKAT NOT = '08'                            
170200       MOVE 2                            TO ANSV-KDCALL                   
170300       MOVE W-IDDISTR                    TO ANSV-IDDISTR                  
170302       MOVE W-IDKUNDNR                   TO ANSV-IDKUNDNR                 
170306       MOVE LEV-IDDC                     TO ANSV-IDDC                     
170400       MOVE NEJ                          TO AUTO-REM-SW                   
170503       MOVE LEV-IDFTG                    TO ANSV-IDFTG                    
170603       MOVE LEV-KDANMORS                 TO ANSV-KDANMORS                 
170703       MOVE LEV-IDFAKT                   TO W-IDFAKT                      
170906                                                                          
171003       PERFORM IMS-GU-WDL501                                              
171103       IF SEGMENT-FINNS                                                   
171203                                                                          
171303         MOVE W-IDDISTR                  TO W-IDDISTR-L5                  
171503         MOVE W-IDKUNDNR                 TO W-IDKUNDNR-L5                 
171703         MOVE LEV-IDKUNDRF               TO W-IDKUNDRF-L5                 
171803         MOVE LEV-IDKOLLI                TO W-IDKOLLI                     
171903         PERFORM IMS-GNP-WDL511                                           
172603         IF SEGMENT-FINNS                                                 
172707           MOVE FAKC-KDORDKL             TO ANSV-KDORDKL                  
172801                                                                          
172905           MOVE FAKC-IDPRODNR            TO W-IDPRODNR                    
173003           MOVE LEV-IDARTNR              TO W-IDARTNR                     
173205           PERFORM IMS-GNP-WDL521                                         
173305           IF SEGMENT-FINNS                                               
173407              MOVE FAKL-ADLAGOMR         TO ANSV-ADLAGOMR                 
173505           ELSE                                                           
173605             MOVE ZERO                   TO ANSV-ADLAGOMR                 
173705           END-IF                                                         
173805         ELSE                                                             
173905           MOVE LEV-KDORDKL              TO ANSV-KDORDKL                  
174005           MOVE ZERO                     TO ANSV-ADLAGOMR                 
174105         END-IF                                                           
174205       ELSE                                                               
174305         MOVE LEV-KDORDKL                TO ANSV-KDORDKL                  
174405         MOVE ZERO                       TO ANSV-ADLAGOMR                 
174505       END-IF                                                             
174605                                                                          
174705       CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                         
174805                                         4115-PCB                         
174905                                         4117-PCB                         
175005                                                                          
175105       IF ANSV-KDSVAR = SPACE                                             
175205         MOVE ANSV-KDARBTYP              TO LEV-KDARBTYP                  
175305         MOVE ANSV-IDPERSON              TO LEV-IDPERSON                  
175405         MOVE 'RR '                      TO LEV-KDKREBEH                  
175505         MOVE JA                         TO UPPDAT-STATUS-2-SW            
175605       END-IF                                                             
175705                                                                          
175805     ELSE                                                                 
175905       MOVE 1                            TO ANSV-KDCALL                   
176005       MOVE W-IDDISTR                    TO ANSV-IDDISTR                  
176105       MOVE MID-IDFTG (IX)               TO ANSV-IDFTG                    
176205       MOVE W-IDKUNDNR                   TO ANSV-IDKUNDNR                 
176305       MOVE MID-KDANMORS (IX)            TO ANSV-KDANMORS                 
176405       MOVE ZERO                         TO ANSV-KDORDKL                  
176505                                            ANSV-ADLAGOMR                 
176605                                                                          
176705       CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                         
176805                                         4115-PCB                         
176905                                         4117-PCB                         
177005                                                                          
177105       IF ANSV-KDSVAR = SPACE                                             
177205         MOVE ANSV-KDARBTYP              TO LEV-KDARBTYP                  
177305         MOVE ANSV-IDPERSON              TO LEV-IDPERSON                  
177405       END-IF                                                             
177505     END-IF                                                               
177605                                                                          
177705     IF ANSV-KDSVAR = SPACE                                               
177805       CONTINUE                                                           
177905     ELSE                                                                 
178005       IF ANSV-KDSVAR = 'S'                                               
178105         MOVE HIGH-VALUE                 TO ANSV-KDANMORS                 
178205         MOVE 1                          TO ANSV-KDCALL                   
178305         CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                       
178405         IF ANSV-KDSVAR = SPACE                                           
178505            MOVE ANSV-KDARBTYP           TO LEV-KDARBTYP                  
178605            MOVE ANSV-IDPERSON           TO LEV-IDPERSON                  
178705         ELSE                                                             
178805            MOVE '*** W418ANSV KOD 100 SAKNAS ' TO FELTEXT                
178905            CALL ABEND USING RKOD-ABEND-MED-DUMP                          
179005         END-IF                                                           
179105       ELSE                                                               
179205         MOVE 'W418ANSV *** KDSVAR ÄR FEL  '      TO FELTEXT              
179305         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
179405       END-IF                                                             
179505     END-IF                                                               
179605     .                                                                    
179705     EJECT                                                                
179805 BDA-KOLLA-OM-AUTO-REM SECTION.                                           
179905                                                                          
180005     IF LEV-KDVAT = SPACE AND (DIST79-DEALER-PRICE OR                     
180205                               DIST79-ECOM-PRICE)                         
180305       CONTINUE                                                           
180405     ELSE                                                                 
180505       IF MID-KDKREBEH (IX) = 'Q  ' OR 'P  '                              
180605         CONTINUE                                                         
180705       ELSE                                                               
180805                                                                          
180905         MOVE LEV-IDDC         TO W-4130-IDDC                             
181005         MOVE LEV-KDANMORS     TO W-4132-KDANMORS                         
181105         PERFORM IMS-GU-WDGX4132                                          
181205         IF SEGMENT-FINNS                                                 
181305           MOVE JA             TO AUTO-REM-SW                             
181405         END-IF                                                           
181505       END-IF                                                             
181605     END-IF                                                               
181705     .                                                                    
181805     EJECT                                                                
181905 S01-SKICKA-PRISFRAGA-W30391 SECTION.                                     
182005                                                                          
182105     MOVE 1                     TO 3391-REQU-IDMSGVER                     
182205     MOVE SPACE                 TO 3391-REQU-KDPGMACT                     
182305     MOVE 'W4079100'            TO 3391-REQU-IDUSER                       
182405                                                                          
182505     MOVE MID-IDDISTR           TO 3391-MID-IDDISTR                       
182605     MOVE MID-IDKUNDNR          TO 3391-MID-IDKUNDNR                      
182705     MOVE SPACE                 TO 3391-MID-IDBUNDLE                      
182805     MOVE MID-IDRAPPNR          TO 3391-MID-IDRAPPNR                      
182905     MOVE LEV-IDPRQUES          TO 3391-MID-IDPRQUES                      
183005                                                                          
183105     PERFORM S02-SEND-OPEN                                                
183205     PERFORM S03-SEND-PUT                                                 
183305     PERFORM S04-SEND-CLOSE                                               
183405     .                                                                    
183505     EJECT                                                                
183605 S02-SEND-OPEN SECTION.                                                   
183705                                                                          
183805     MOVE 'OPEN' TO SEND-KDFUNC                                           
183905     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
184005                                                                          
184105     CALL WZ01SEND USING           SEND-CONTROL-AREA                      
184205                                   SEND-OPEN-AREA                         
184305********              ...FELHANTERING...                                  
184405     IF SEND-KDRC > 0                                                     
184505      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
184605      STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                      
184705      DELIMITED BY SIZE INTO FELTEXT                                      
184805      CALL ABEND USING RKOD-ABEND-MED-DUMP                                
184905     END-IF                                                               
185005     .                                                                    
185105     EJECT                                                                
185205 S03-SEND-PUT SECTION.                                                    
185305                                                                          
185405     MOVE 'PUT'                           TO SEND-KDFUNC                  
185505     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
185605     CALL WZ01SEND USING SEND-CONTROL-AREA                                
185705                         SEND-KVDLEN                                      
185805                         SEND-AREA                                        
185905**FELHANTERING...                                                         
186005     IF SEND-KDRC > 1                                                     
186105       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
186205       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
186305       DELIMITED BY SIZE INTO FELTEXT                                     
186405       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
186505     END-IF                                                               
186605     .                                                                    
186705     EJECT                                                                
186805 S04-SEND-CLOSE SECTION.                                                  
186905                                                                          
187005     MOVE 'CLOSE' TO SEND-KDFUNC                                          
187105     CALL WZ01SEND USING SEND-CONTROL-AREA                                
187205**FELHANTERING...                                                         
187305     IF SEND-KDRC > 0                                                     
187405      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
187505      STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                     
187605       DELIMITED BY SIZE INTO FELTEXT                                     
187705       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
187805     END-IF                                                               
187905                                                                          
188005     ADD +1                           TO WS-ANTAL-SEND                    
188105     .                                                                    
188205     EJECT                                                                
188305 S05-HAEMTA-IDUSER-BEANST SECTION.                                        
188405                                                                          
188505      MOVE MID-IDDISTR                 TO W-IDDISTR-4124                  
188605                                                                          
188705      PERFORM IMS-GU-WDR101                                               
188805      PERFORM IMS-GNP-WDGX4124                                            
188905      IF SEGMENT-FINNS                                                    
189005        MOVE 4124-IDDISTR-FOM TO W-4124-IDDISTR-FOM                       
189105        MOVE 4124-IDDISTR-TOM TO W-4124-IDDISTR-TOM                       
189205        PERFORM IMS-GNP-WDGX4126                                          
189305        IF SEGMENT-FINNS                                                  
189405          MOVE 4126-BEANST             TO ANM-BEANST                      
189505          MOVE 4126-IDUSER-ADM         TO ANM-IDUSER-ADM                  
189605        END-IF                                                            
189705      END-IF                                                              
189805                                                                          
189905      IF SEGMENT-SAKNAS                                                   
190005        MOVE +9999                    TO W-4124-IDDISTR-FOM               
190105                                          W-4124-IDDISTR-TOM              
190205        MOVE +1                        TO W-4126-SUKRENOT-FOM             
190305        MOVE +9999999                  TO W-4126-SUKRENOT-TOM             
190405                                                                          
190505        PERFORM IMS-GU-WDGX4126                                           
190605        IF SEGMENT-FINNS                                                  
190705          MOVE 4126-BEANST             TO ANM-BEANST                      
190805          MOVE 4126-IDUSER-ADM         TO ANM-IDUSER-ADM                  
190905        ELSE                                                              
191005          MOVE SPACE                   TO ANM-BEANST                      
191105          MOVE SPACE                   TO ANM-IDUSER-ADM                  
191205        END-IF                                                            
191305      END-IF                                                              
191405     .                                                                    
191505     EJECT                                                                
191605 Z-FINIT SECTION.                                                         
191705                                                                          
191805     IF WS-IDPRQUES > ZERO                                                
191905       MOVE +3                TO PRNO-KDCALL                              
192005       MOVE WS-IDPRQUES       TO PRNO-IDPRQUES-IN                         
192105       CALL W335PRNO USING PRNO-W335PRNO PRNO-PCB                         
192205     END-IF                                                               
192305                                                                          
192405     IF MSG-KOM-IDMFSMED = SPACE                                          
192505       MOVE OK-BEHANDLAD                 TO MSG-KOM-IDMFSMED              
192605     ELSE                                                                 
192705       MOVE ERR-OTILL-UPPDAT             TO MSG-KOM-IDMFSMED              
192805       MOVE '4'                          TO MSG-KOM-KDSVAR                
192905     END-IF                                                               
193005                                                                          
193105     PERFORM IMS-INSERT-DISP-MSG                                          
193205                                                                          
193305     .                                                                    
193405     EJECT                                                                
193505* --- IMS SEKTIONER ---                                                   
193605     SKIP3                                                                
193705 IMS-GET-MSG SECTION.                                                     
193805                                                                          
193905     MOVE '  QC' TO GODK-STATUSKODER                                      
194005     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
194105     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
194205     PERFORM IMS-STATUSKONTROLL                                           
194305     .                                                                    
194405     SKIP3                                                                
194505 IMS-GN-MSG SECTION.                                                      
194605                                                                          
194705     MOVE SPACE TO GODK-STATUSKODER                                       
194805     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
194905     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
195005     PERFORM IMS-STATUSKONTROLL                                           
195105     .                                                                    
195205     SKIP3                                                                
195305 IMS-INSERT-DISP-MSG SECTION.                                             
195405                                                                          
195505     MOVE SPACE TO GODK-STATUSKODER                                       
195605     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
195705     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
195805     PERFORM IMS-STATUSKONTROLL                                           
195905     .                                                                    
196005     EJECT                                                                
196105 IMS-GU-WDA2-ANM SECTION.                                                 
196205                                                                          
196305     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
196405          DELIMITED BY SIZE INTO SSA1                                     
196505     MOVE '  GE' TO GODK-STATUSKODER                                      
196605     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-AREA SSA1                      
196705     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
196805     PERFORM IMS-STATUSKONTROLL                                           
196905     .                                                                    
197005     EJECT                                                                
197105 IMS-GNP-WDA2-LEV SECTION.                                                
197205                                                                          
197305     MOVE 'WDA211  ' TO SSA1                                              
197405     MOVE '  GE' TO GODK-STATUSKODER                                      
197505     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-AREA SSA1                     
197605     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
197705     PERFORM IMS-STATUSKONTROLL                                           
197805     .                                                                    
197905     EJECT                                                                
198005 IMS-GHU-WDA2-ANM SECTION.                                                
198105                                                                          
198205     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
198305          DELIMITED BY SIZE INTO SSA1                                     
198405     MOVE '  GE' TO GODK-STATUSKODER                                      
198505     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-AREA SSA1                     
198605     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
198705     PERFORM IMS-STATUSKONTROLL                                           
198805     .                                                                    
198905     EJECT                                                                
199005 IMS-REPL-WDA2 SECTION.                                                   
199105                                                                          
199205     MOVE '    ' TO GODK-STATUSKODER                                      
199305     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-AREA                         
199405     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
199505     PERFORM IMS-STATUSKONTROLL                                           
199605     .                                                                    
199705     EJECT                                                                
199805 IMS-ISRT-WDA2-ROT SECTION.                                               
199905                                                                          
200005     MOVE 'WDA201  ' TO SSA1                                              
200105     MOVE '    ' TO GODK-STATUSKODER                                      
200205     CALL CBLTDLI USING ISRT WDA2-PCB DLI-IO-AREA SSA1                    
200305     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
200405     PERFORM IMS-STATUSKONTROLL                                           
200505     .                                                                    
200605     EJECT                                                                
200705 IMS-ISRT-WDA2-RAD SECTION.                                               
200805                                                                          
200905     MOVE 'WDA211  ' TO SSA1                                              
201005     MOVE '  II' TO GODK-STATUSKODER                                      
201105     CALL CBLTDLI USING ISRT WDA2-PCB DLI-IO-AREA SSA1                    
201205     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
201305     PERFORM IMS-STATUSKONTROLL                                           
201405     .                                                                    
201505     EJECT                                                                
201605 IMS-GET-WDB201                 SECTION.                                  
201705                                                                          
201805     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
201905            DELIMITED BY SIZE INTO SSA1                                   
202005     MOVE '  GE' TO GODK-STATUSKODER                                      
202105     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                 
202205     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
202305     PERFORM IMS-STATUSKONTROLL                                           
202405     .                                                                    
202505     EJECT                                                                
202605 IMS-GET-WDB101                 SECTION.                                  
202705                                                                          
202805     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
202905            DELIMITED BY SIZE INTO SSA1                                   
203005     MOVE '  GE' TO GODK-STATUSKODER                                      
203105     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB1 SSA1                 
203205     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
203305     PERFORM IMS-STATUSKONTROLL                                           
203405     .                                                                    
203505     EJECT                                                                
203605 IMS-GU-WDC701 SECTION.                                                   
203705                                                                          
203805     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
203905          DELIMITED BY SIZE INTO SSA1                                     
204005     MOVE '  GE' TO GODK-STATUSKODER                                      
204105     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
204205     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
204305     PERFORM IMS-STATUSKONTROLL                                           
204405     .                                                                    
204505     SKIP3                                                                
204605 IMS-ISRT-WDC701 SECTION.                                                 
204705                                                                          
204805     MOVE 'WDC701  ' TO SSA1                                              
204905     MOVE '    ' TO GODK-STATUSKODER                                      
205005     CALL CBLTDLI USING ISRT WDC7-PCB DLI-IO-WDC701 SSA1                  
205105     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
205205     PERFORM IMS-STATUSKONTROLL                                           
205305     .                                                                    
205405     EJECT                                                                
205505 IMS-ISRT-WDC711 SECTION.                                                 
205605                                                                          
205705     MOVE 'WDC711  ' TO SSA1                                              
205805     MOVE '    ' TO GODK-STATUSKODER                                      
205905     CALL CBLTDLI USING ISRT WDC7-PCB DLI-IO-WDC711 SSA1                  
206005     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
206105     PERFORM IMS-STATUSKONTROLL                                           
206205     .                                                                    
206305     EJECT                                                                
206405 IMS-GU-WDR101 SECTION.                                                   
206505                                                                          
206605     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
206705            DELIMITED BY SIZE INTO SSA1                                   
206805     MOVE '  ' TO GODK-STATUSKODER                                        
206905     CALL CBLTDLI USING GU 4123-PCB DLI-IO-WDR101 SSA1                    
207005     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
207105     PERFORM IMS-STATUSKONTROLL                                           
207205     .                                                                    
207305     EJECT                                                                
207405 IMS-GNP-WDGX4124 SECTION.                                                
207505                                                                          
207605     STRING 'WDGX4124(IDDISTRF<=' W-IDDISTR-X                             
207705                    '&IDDISTRT>=' W-IDDISTR-X ')'                         
207805            DELIMITED BY SIZE INTO SSA1                                   
207905     MOVE '  GE' TO GODK-STATUSKODER                                      
208005     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
208105     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
208205     PERFORM IMS-STATUSKONTROLL                                           
208305     .                                                                    
208405     SKIP3                                                                
208505 IMS-GNP-WDGX4126 SECTION.                                                
208605                                                                          
208705     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
208805          DELIMITED BY SIZE INTO SSA1                                     
208905     MOVE 'WDGX4126 ' TO SSA2                                             
209005     MOVE '  GE' TO GODK-STATUSKODER                                      
209105     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2            
209205     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
209305     PERFORM IMS-STATUSKONTROLL                                           
209405     .                                                                    
209505     SKIP3                                                                
209605 IMS-GU-WDGX4126 SECTION.                                                 
209705                                                                          
209805     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
209905            DELIMITED BY SIZE INTO SSA1                                   
210005     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
210105          DELIMITED BY SIZE INTO SSA2                                     
210205     STRING 'WDGX4126(KEY4126  =' W-KEY4126-X ')'                         
210305            DELIMITED BY SIZE INTO SSA3                                   
210405     MOVE '  GE' TO GODK-STATUSKODER                                      
210505     CALL CBLTDLI USING GU 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2             
210605                                                     SSA3                 
210705     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
210805     PERFORM IMS-STATUSKONTROLL                                           
210905     .                                                                    
211005     SKIP3                                                                
211105 IMS-GU-WDR501-4103-GE SECTION.                                           
211205                                                                          
211305     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
211405          DELIMITED BY SIZE INTO SSA1                                     
211505     MOVE '  GE' TO GODK-STATUSKODER                                      
211605     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
211705     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
211805     PERFORM IMS-STATUSKONTROLL                                           
211905     .                                                                    
212005     SKIP3                                                                
212105 IMS-ISRT-WDR501-4103 SECTION.                                            
212205                                                                          
212305     MOVE 'WDR501  ' TO SSA1                                              
212405     MOVE '    ' TO GODK-STATUSKODER                                      
212505     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4103 SSA1                
212605     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
212705     PERFORM IMS-STATUSKONTROLL                                           
212805     .                                                                    
212905     EJECT                                                                
213005 IMS-GU-WDR501-4103 SECTION.                                              
213105                                                                          
213205     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
213305          DELIMITED BY SIZE INTO SSA1                                     
213405     MOVE '    ' TO GODK-STATUSKODER                                      
213505     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
213605     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
213705     PERFORM IMS-STATUSKONTROLL                                           
213805     .                                                                    
213905     SKIP3                                                                
214005 IMS-GHNP-WDGX4104 SECTION.                                               
214105                                                                          
214205     STRING 'WDGX4104*F(KEY4104  =' W-KEY4104-X ')'                       
214305          DELIMITED BY SIZE INTO SSA1                                     
214405     MOVE '  GE' TO GODK-STATUSKODER                                      
214505     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
214605     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
214705     PERFORM IMS-STATUSKONTROLL                                           
214805     .                                                                    
214905     SKIP3                                                                
215005 IMS-ISRT-WDGX4104 SECTION.                                               
215105                                                                          
215205     MOVE 'WDGX4104' TO SSA1                                              
215305     MOVE '    ' TO GODK-STATUSKODER                                      
215405     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4104 SSA1                
215505     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
215605     PERFORM IMS-STATUSKONTROLL                                           
215705     .                                                                    
215805     EJECT                                                                
215905 IMS-REPL-WDGX4104 SECTION.                                               
216005                                                                          
216105     MOVE 'WDGX4104' TO SSA1                                              
216205     MOVE '    ' TO GODK-STATUSKODER                                      
216305     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104 SSA1                
216405     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
216505     PERFORM IMS-STATUSKONTROLL                                           
216605     .                                                                    
216705     EJECT                                                                
216805 IMS-GU-WDL501      SECTION.                                              
216905                                                                          
217006     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
217105          DELIMITED BY SIZE INTO SSA1                                     
217205     MOVE '  GE'           TO GODK-STATUSKODER                            
217305     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
217405     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
217505     PERFORM IMS-STATUSKONTROLL                                           
217605     .                                                                    
217705     EJECT                                                                
217805 IMS-GNP-WDL511      SECTION.                                             
217905                                                                          
218003     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
218103                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
218203          DELIMITED BY SIZE INTO SSA1                                     
218303     MOVE '  GE'           TO GODK-STATUSKODER                            
218403     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
218503     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
218603     PERFORM IMS-STATUSKONTROLL                                           
218703     .                                                                    
218803     EJECT                                                                
218903 IMS-GNP-WDL521     SECTION.                                              
219003                                                                          
219105     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
219303          DELIMITED BY SIZE INTO SSA1                                     
219403     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-X ')'                         
219603          DELIMITED BY SIZE INTO SSA2                                     
219703     MOVE '  GE'           TO GODK-STATUSKODER                            
219803     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
219903     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
220003     PERFORM IMS-STATUSKONTROLL                                           
220103     .                                                                    
220203     EJECT                                                                
220303 IMS-GU-WDGX4132       SECTION.                                           
220403                                                                          
220503     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
220603         DELIMITED BY SIZE INTO SSA1                                      
220703     STRING 'WDGX4130(IDDC     =' W-WDGXKEY-4130-X ')'                    
220803         DELIMITED BY SIZE INTO SSA2                                      
220903     STRING 'WDGX4132(KDANMORS =' W-WDGXKEY-4132-X ')'                    
221003         DELIMITED BY SIZE INTO SSA3                                      
221103     MOVE '  GE' TO GODK-STATUSKODER                                      
221203     CALL CBLTDLI USING GU 4129-PCB 4132-WDGX4132 SSA1 SSA2 SSA3          
221303     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
221403     PERFORM IMS-STATUSKONTROLL                                           
221503     .                                                                    
221603     SKIP3                                                                
221703 IMS-STATUSKONTROLL SECTION.                                              
221803                                                                          
221903     SET STATUS-IX TO 1                                                   
222003     SEARCH GODK-STATUS                                                   
222103       AT END                                                             
222203         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
222303         DELIMITED BY SIZE INTO FELTEXT                                   
222403         CALL FELLOG                                                      
222503       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
222603         CONTINUE                                                         
223003     END-SEARCH                                                           
230001     .                                                                    
