000100 ID  DIVISION.                                                            
000201 PROGRAM-ID.    W4768000.                                                 
000301 AUTHOR.        CAMELIA OLGRENER.                                         
000401 DATE-WRITTEN.  APRIL 2003.                                               
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801*    FUNKTION:                                                            
000901*        PROGRAMMET STARTAS FRÅN MPP 4538                                 
001001*        VIA SOP-RUTIN W476SE                                             
001101*        IDTULL ÄR PARAMETER IN                                           
001201*                                                                         
001301*        PGM:ET LÄSER:                                                    
001401*        * WDM7, WDM8, WDB5, WDE6                                         
001501*        NY FIL TILL MIC (FEBR. 2019), LÄSER ÄVEN:                        
001601*        * WDE2, WDK6, WDB2, WDR1/4735 (LEV.VILLKOR-TEXT)                 
001701*                                                                         
001801*        SKAPAR FIL MED INFO TILL TULLSYST.                               
001901     EJECT                                                                
002001 ENVIRONMENT DIVISION.                                                    
002101     SKIP2                                                                
002201 INPUT-OUTPUT SECTION.                                                    
002301                                                                          
002401 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002601*- - - - - - - - - - - - IDTULL PARAMETER FRÅN 4538                       
002701     SELECT W476IN                       ASSIGN TO W47680D1.              
002801*- - - - - - - - - - - - UTFIL:                                           
002901*          --- FIL TULLINFO                                               
003001     SELECT W47680                       ASSIGN TO W47680D2.              
003101     SKIP2                                                                
003201 DATA DIVISION.                                                           
003301     SKIP2                                                                
003401 FILE SECTION.                                                            
003501 FD  W476IN                                                               
003601     RECORDING      F                                                     
003701     BLOCK CONTAINS 0.                                                    
003801 01  PARM            PIC X(80).                                           
003901 FD  W47680                                                               
004001     RECORDING       V                                                    
004101     BLOCK CONTAINS  0.                                                   
004201 01  UT-HUV.                                                              
004301*03   -COPY W476TU1       -L.                                             
004401     SKIP2                                                                
004501 01  UT-RAD.                                                              
004601*03   -COPY W476TU2       -L.                                             
004701 WORKING-STORAGE SECTION.                                                 
004801                                                                          
004901*    -- CHECKED BY WY2000                                                 
005001 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4768000'.               
006001                                                                          
006101 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
006201 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
006301 77  WS-SUORDV-FAKT              PIC S9(9)V9(2) VALUE +0 COMP-3.          
006401 77  WS-VKRAD-NTO-TOT            PIC S9(6)V9(3) VALUE +0 COMP-3.          
006501 77  WS-VKORDBTO-TOT             PIC S9(6)V9(3) VALUE +0 COMP-3.          
006601 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0 COMP-3.          
006701 77  WS-KVKOLLI                  PIC S9(5)      VALUE +0 COMP-3.          
006801 77  WS-KVKOLLI                  PIC S9(5)      VALUE +0 COMP-3.          
006901 77  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
007001 77  WS-KVKOLLI-TULL             PIC S9(5)      VALUE +0 COMP-3.          
007101 77  WS-KVFAKTUR                 PIC S9(3)      VALUE +0 COMP-3.          
007201 77  SPAR-IDFAKT                 PIC S9(7)      VALUE +0 COMP-3.          
007301 77  SPAR-IDFAKT-MIC             PIC X(24)      VALUE SPACE.              
007401 77  WS-CUST-CNT                 PIC 9(3)       VALUE 0.                  
007501 77  WS-PSN-NAME                 PIC X(75)      VALUE SPACE.              
007601 77  WS-IDSIGILL                 PIC X(15)      VALUE SPACE.              
007701 77  ROW-IX                      PIC 9(3)       VALUE 0.                  
007801 77  CUST-IX                     PIC 9(3)       VALUE 0.                  
007901 77  MAX-CUST                    PIC 9(3)       VALUE 200.                
008001                                                                          
008101 77  WDE211-SW                   PIC X       VALUE 'N'.                   
008201     88 WDE211-OK                            VALUE 'J'.                   
008301                                                                          
008401 77  INFIL-EOF-SW                PIC X    VALUE 'N'.                      
008501     88  END-OF-W476IN                    VALUE 'J'.                      
008601                                                                          
008701 77  CUST-FND-SW                 PIC X    VALUE 'N'.                      
008801     88  CUST-FND                         VALUE 'J'.                      
008901                                                                          
009001 77  OLD-INVOICE-SW              PIC X    VALUE 'N'.                      
009101     88  OLD-INVOICE                      VALUE 'J'.                      
009201                                                                          
009301 77  SEND-UN-NUM-SW              PIC X    VALUE 'N'.                      
009401     88  SEND-UN-NUM                      VALUE 'J'.                      
009501                                                                          
009601 01  WS-IDKOLLI                  PIC X(5).                                
009701 01  WS-IDKOLLI-NUM REDEFINES WS-IDKOLLI.                                 
009801     03  WS-IDKOLLI-POS1-2       PIC 9(2).                                
009901     03  WS-IDKOLLI-POS3         PIC 9(1).                                
010001     03  WS-IDKOLLI-POS4         PIC 9(1).                                
010101     03  WS-IDKOLLI-POS5         PIC 9(1).                                
010201                                                                          
010301 01  WS-IDKOLLI-MIC              PIC X(23).                               
010401 01  FILLER REDEFINES WS-IDKOLLI-MIC.                                     
010501     03  WS-IDDISTR              PIC 9(4).                                
010601     03  WS-IDKUNDNR             PIC 9(7).                                
010701     03  WS-IDORDNR7             PIC 9(7).                                
010801     03  WS-IDKOLLI-M            PIC 9(5).                                
010901                                                                          
011001 01 CUST-PRICE-TABLE.                                                     
011101    05 CUST-PRICE OCCURS 1 TO 200 DEPENDING ON WS-CUST-CNT.               
011201       07 TEMP-IDKUNDNR        PIC S9(7)      COMP-3 VALUE +0.            
011301       07 TEMP-PREMBHNT        PIC S9(7)V9(2) COMP-3 VALUE +0.            
011401       07 TEMP-PRFRAKT         PIC S9(7)V9(2) COMP-3 VALUE +0.            
011501       07 TEMP-PRFOERS         PIC S9(7)V9(2) COMP-3 VALUE +0.            
011601       07 TEMP-PRLEGKST        PIC S9(7)V9(2) COMP-3 VALUE +0.            
011701                                                                          
011801*VALID PSN LIST FOR DIFF TRANSPORT                                        
011901*KDTRPTYP = 2 OR 3                                                        
012001 01  GOOD-PSN-ADR-SW              PIC 9(3).                               
012101    88  GOOD-PSN-ADR                       VALUE 010                      
012201                                                 022 024                  
012301                                                 030                      
012401                                                 053                      
012501                                                 060                      
012601                                                 080 088 089              
012701                                                 090 091 097.             
012801                                                                          
012901*KDTRPTYP = 4                                                             
013001 01  GOOD-PSN-DGR-SW              PIC 9(3).                               
013101     88  GOOD-PSN-DGR                       VALUE 010                     
013201                                                  020 021 022 023         
013301                                                  024 025                 
013401                                                  031 032 033 034         
013501                                                  035 036 037 038         
013601                                                  039                     
013701                                                  040 041 042 043         
013801                                                  044 045 046 047         
013901                                                  048 049                 
014001                                                  050 051 052 053         
014101                                                  054 055 056 057         
014201                                                  058 059                 
014301                                                  061 062 063 064         
014401                                                  065 THRU 069            
014501                                                  070 THRU 075            
014601                                                  080 081 088 089         
014701                                                  090 091 092 093         
014801                                                  094 THRU 099            
014901                                                  110 THRU 129            
015001                                                  133                     
015101                                                  135 THRU 138            
015201                                                  140                     
015301                                                  143 THRU 147            
015401                                                  149 THRU 332            
015501                                                  334 THRU 399.           
015601                                                                          
015701*KDTRPTYP = 1                                                             
015801 01  GOOD-PSN-IMDG-SW             PIC 9(3).                               
015901     88  GOOD-PSN-IMDG                      VALUE 010                     
016001                                                  020 021 022 023         
016101                                                  024 025                 
016201                                                  030 031 032 033         
016301                                                  034 035 036 037         
016401                                                  038 039                 
016501                                                  040 041 042 043         
016601                                                  044 045 046 047         
016701                                                  049                     
016801                                                  050 051 052 053         
016901                                                  054 055 056 057         
017001                                                  058 059                 
017101                                                  060 061 062 063         
017201                                                  064 THRU 069            
017301                                                  070 THRU 075            
017401                                                  080 081 088 089         
017501                                                  090 091 092 093         
017601                                                  094 095 096 097         
017701                                                  907.                    
017801 01  FILLER                      PIC X(16)  VALUE 'WS-SEKTION'.           
017901 01  WS-SEKTION                  PIC X(30)  VALUE SPACE.                  
018001                                                                          
018101 01  FELTEXT.                                                             
018201     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018301     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018401     EJECT                                                                
018501 01  TEST-IDDISTR     PIC S9(5)        VALUE ZERO  COMP-3.                
018601*01  FILLER     -COPY WWDIST25   -RED  TEST-IDDISTR.                      
018701     EJECT                                                                
018702*01  FILLER     -COPY WWDIST35   -RED  TEST-IDDISTR.                      
018703     EJECT                                                                
018801*01  FILLER     -COPY WWDIST41   -RED  TEST-IDDISTR.                      
018901     EJECT                                                                
019001*01  FILLER     -COPY WWDIST42   -RED  TEST-IDDISTR.                      
019101     EJECT                                                                
019201*01  FILLER     -COPY WWDIST76   -RED  TEST-IDDISTR.                      
019301     EJECT                                                                
019401******************************************************************        
019501*       CONSTANTS                                                *        
019601******************************************************************        
019701     SKIP2                                                                
019801 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
019901 01  KONSTANTER.                                                          
020001     03  JA                      PIC X(1)    VALUE 'J'.                   
020101     03  NEJ                     PIC X(1)    VALUE 'N'.                   
020201     SKIP2                                                                
020301                                                                          
020401 01  SPAR-IDDISTR                PIC S9(5)  VALUE ZERO COMP-3.            
020501                                                                          
020601     SKIP3                                                                
020701******************************************************************        
020801*       VARIABLES                                                *        
020901******************************************************************        
021001     SKIP2                                                                
021101 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
021201 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
021301     SKIP3                                                                
021401     EJECT                                                                
021501*    --- VALID IDDC CODES                                                 
021601*                                                                         
021701*    -COPY WWDCKONS                                                       
021801     EJECT                                                                
021901 01  DYNAMISKA-SUBPROGRAM.                                                
022001   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
022101   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
022201   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
022301   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
022401     SKIP2                                                                
022501 01  RETURKODER.                                                          
022601     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
022701     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
022801     SKIP2                                                                
022901*01   -COPY W0005       -PRE POSTSUM-.                                    
023001     EJECT                                                                
023101 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
023201 01  IN-AREA                       PIC X(80) VALUE SPACE.                 
023301 01  FILLER                        REDEFINES IN-AREA.                     
023401     03  IN-IDTULL                 PIC X(10).                             
023501     EJECT                                                                
023601 01  FILLER                      PIC X(16)   VALUE                        
023701                                 'ARB-HUV-UTAREA'.                        
023801     SKIP2                                                                
023901 01  ARB-HUV-UTAREA.                                                      
024001*    03     -COPY W476TU1  -PRE UT-.                                      
024101     EJECT                                                                
024201*                                                                         
024301 01  ARB-RAD-UTAREA.                                                      
024401*    03     -COPY W476TU2  -PRE UT-.                                      
024501     EJECT                                                                
024601 01  FILLER                      PIC X(16)   VALUE                        
024701                                 'ARB-HUV-UTAREA  '.                      
024801     SKIP2                                                                
024901 01  RENS-AREA-START             PIC X(16)   VALUE                        
025001                                 'RENS-AREA-START '.                      
025101 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025201     SKIP3                                                                
025301 01  NYCKLAR-TILL-DLI.                                                    
025401                                                                          
025501     03  W-WDM7BSEQ-MIN-X.                                                
025601         05  W-IDTULL-MIN        PIC X(10)   VALUE ZERO.                  
025701         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
025801                                                                          
025901     03  W-WDM7BSEQ-MAX-X.                                                
026001         05  W-IDTULL-MAX        PIC X(10)   VALUE ZERO.                  
026101         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
026201                                                                          
026301     03  W-WDM801KY-MIN-X.                                                
026401         05  W-IDFAKT-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
026501         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
026601         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
026701         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
026801                                                                          
026901     03  W-WDM801KY-MAX-X.                                                
027001         05  W-IDFAKT-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
027101         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
027201         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
027301         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
027401                                                                          
027501     03  W-WDB501KY-X.                                                    
027601         05  W-IDDC              PIC X(2).                                
027701         05  W-KDFRAKT           PIC S9(3)   COMP-3.                      
027801         05  W-IDGMT.                                                     
027901           07 W-IDDISTR          PIC S9(5)   COMP-3.                      
028001           07 W-IDKUNDNR         PIC S9(7)   COMP-3.                      
028101                                                                          
028201     03  W-WDB501KY-DEF.                                                  
028301         05  W-IDDC-DEF          PIC X(2).                                
028401         05  W-KDFRAKT-DEF       PIC S9(3)   COMP-3.                      
028501         05  W-IDGMT-DEF.                                                 
028601           07 W-IDDISTR-DEF      PIC S9(5)   COMP-3.                      
028701           07 W-IDKUNDNR-DEF     PIC S9(7) COMP-3  VALUE +9999999.        
028801                                                                          
028901     03  W-IDARTNR-X.                                                     
029001         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029101                                                                          
029201     03  W-KDSEGKEY-X.                                                    
029301         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
029401                                                                          
029501     03  W-IDPRODNR-X.                                                    
029601         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
029701                                                                          
029801     03  W-IDKOLLI-X.                                                     
029901         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
030001                                                                          
030101     03  W-IDSHIPM-X.                                                     
030201         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
030301                                                                          
030401     03  W-WDE211KY-X.                                                    
030501         05  W-IDDISTR-E2        PIC S9(5)   VALUE ZERO COMP-3.           
030601         05  W-IDKUNDNR-E2       PIC S9(7)   VALUE ZERO COMP-3.           
030701                                                                          
030801     03 W-WDGXKEY-4735-X.                                                 
030901         05 IDHTYP-4735          PIC X(4)    VALUE '4735'.                
031001         05 KDLEVVIL-4735        PIC S9(3)   VALUE ZERO  COMP-3.          
031101         05 FILLER               PIC X(24)   VALUE LOW-VALUE.             
031201                                                                          
031301     03  W-IDGMT-X.                                                       
031401       05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO COMP-3.           
031501       05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO COMP-3.           
031601*                                                                         
031701     03  W-1165KEY-X.                                                     
031801         05  W-1165              PIC X(4)    VALUE '1165'.                
031901         05  W-IDPSN             PIC 9(3)    VALUE ZERO.                  
032001         05  W-IDSPRAK           PIC X(2)    VALUE 'GB'.                  
032101         05  FILLER              PIC X(21)   VALUE LOW-VALUE.             
032201     03  W-KDFGTRP-X.                                                     
032301         05  W-KDFGTRP           PIC 9(02)   VALUE ZERO.                  
032401                                                                          
032501     EJECT                                                                
032601*    --- STATUS-KOD FRÅN IMS                                              
032701 01  STATUS-WS                   PIC XX.                                  
032801     88  SEGMENT-FINNS                       VALUE '  '.                  
032901     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033001     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033101     88  BASEN-SLUT                          VALUE 'GB'.                  
033201     SKIP2                                                                
033301 01  STATUS-WS-E6                PIC XX.                                  
033401     88  SEGMENT-FINNS-E6                    VALUE '  '.                  
033501     SKIP2                                                                
033601 01  STATUS-WS-K6                PIC XX.                                  
033701     88  SEGMENT-FINNS-K6                    VALUE '  '.                  
033801     SKIP2                                                                
033901 01  GODK-STATUSKODER.                                                    
034001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034101     SKIP3                                                                
034201 01  SSA1                        PIC X(128).                              
034301 01  SSA2                        PIC X(64).                               
034401 01  SSA3                        PIC X(64).                               
034501     EJECT                                                                
034601*    --- IMS FUNKTIONSKODER                                               
034701*01  -COPY W0003                                                          
034801     EJECT                                                                
034901*    ---  DLI INPUT-OUTPUT AREA                                           
035001 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM701'.         
035101 01  DLI-IO-WDM701.                                                       
035201*    03  -COPY WDM701                                                     
035301     EJECT                                                                
035401 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM801'.         
035501 01  DLI-IO-WDM801.                                                       
035601*    03  -COPY WDM801                                                     
035701     EJECT                                                                
035801 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB501'.         
035901 01  DLI-IO-WDB501.                                                       
036001*    03  -COPY WDB501                                                     
036101     EJECT                                                                
036201 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE601'.         
036301 01  DLI-IO-WDE601.                                                       
036401*    03  -COPY WDE601                                                     
036501     EJECT                                                                
036601 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE611'.         
036701 01  DLI-IO-WDE611.                                                       
036801*    03  -COPY WDE611                                                     
036901     EJECT                                                                
037001 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
037101 01  DLI-IO-WDK601.                                                       
037201*    03  -COPY WDK601                                                     
037301     EJECT                                                                
037401 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
037501 01  DLI-IO-WDK611.                                                       
037601*    03  -COPY WDK611                                                     
037701     EJECT                                                                
037801 01  DLI-IO-WDGX4735.                                                     
037901*    03  -COPY WDGX4735.                                                  
038001     EJECT                                                                
038101 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
038201 01  DLI-IO-WDB201.                                                       
038301*     03  -COPY WDB201.                                                   
038401     EJECT                                                                
038501 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE211'.         
038601 01  DLI-IO-WDE211.                                                       
038701*     03  -COPY WDE211.                                                   
038801     EJECT                                                                
038901 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WL1165'.         
039001 01  DLI-IO-WL1165.                                                       
039101*     03 -COPY WDGX1165                                                   
039201     EJECT                                                                
039301 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WL1168'.         
039401 01  DLI-IO-WL1168.                                                       
039501*     03 -COPY WDGX1168                                                   
039601     EJECT                                                                
039701 LINKAGE SECTION.                                                         
039801*01  -COPY W0008  -PRE WDM7-                                              
039901     05  FILLER                  PIC X.                                   
040001*01  -COPY W0008  -PRE WDM8-                                              
040101     05  FILLER                  PIC X.                                   
040201*01  -COPY W0008  -PRE WDB5-                                              
040301     05  FILLER                  PIC X.                                   
040401*01  -COPY W0008  -PRE WDE6-                                              
040501     05  FILLER                  PIC X.                                   
040601*01  -COPY W0008  -PRE WDK6-                                              
040701     05  FILLER                  PIC X.                                   
040801*01  -COPY W0008  -PRE WDE2-                                              
040901     05  FILLER                  PIC X.                                   
041001*01  -COPY W0008  -PRE 4735-                                              
041101     05  FILLER                  PIC X.                                   
041201*01  -COPY W0008  -PRE WDB2-                                              
041301     05  FILLER                  PIC X.                                   
041401*01  -COPY W0008  -PRE 1165-                                              
041501     05  FILLER                  PIC X.                                   
041601     EJECT                                                                
041701 PROCEDURE DIVISION  USING WDM7-PCB WDM8-PCB WDB5-PCB WDE6-PCB            
041801                           WDK6-PCB WDE2-PCB 4735-PCB WDB2-PCB            
041901                           1165-PCB.                                      
042001 MAIN SECTION.                                                            
042101     ENTRY 'DLITCBL' USING WDM7-PCB WDM8-PCB WDB5-PCB WDE6-PCB            
042201                           WDK6-PCB WDE2-PCB 4735-PCB WDB2-PCB            
042301                           1165-PCB.                                      
042401                                                                          
042501     PERFORM A-INIT                                                       
042601     PERFORM B-LAES-PARAMETER                                             
042701                                                                          
042801     PERFORM IMS-GU-WDM701                                                
042901     IF SEGMENT-FINNS                                                     
043001       PERFORM S01-SPARA-HUV                                              
043101                                                                          
043201       MOVE +1         TO CUST-IX                                         
043301       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
043401         MOVE HUV-IDDISTR      TO W-IDDISTR-WDB2                          
043501         MOVE HUV-IDKUNDNR     TO W-IDKUNDNR-WDB2                         
043601         PERFORM IMS-GU-WDB201                                            
043701                                                                          
043801         IF HUV-IDFAKT = SPAR-IDFAKT                                      
043901           PERFORM B-SKRIV-RADFIL                                         
044001           PERFORM C-SKAPA-KOLLI-TOT                                      
044101           IF OLD-INVOICE-SW = NEJ                                        
044201              PERFORM D-SAVE-ADD-COSTS-DIFF-CUST                          
044301           END-IF                                                         
044401           IF HUV-IDSIGILL NOT = SPACE AND                                
044501              WS-IDSIGILL      = SPACE                                    
044601              MOVE HUV-IDSIGILL   TO WS-IDSIGILL                          
044701           END-IF                                                         
044801                                                                          
044901           PERFORM IMS-GN-WDM701                                          
045001           PERFORM F-KOLLA-OM-FLER-FAKT                                   
045101         ELSE                                                             
045201           PERFORM E-SKRIV-FAKT-HUV                                       
045301           PERFORM S01-SPARA-HUV                                          
045401         END-IF                                                           
045501       END-PERFORM                                                        
045601                                                                          
045701       PERFORM G-SKRIV-SISTA-FAKT-HUV                                     
045801                                                                          
045901     END-IF                                                               
046001     PERFORM Z-FINIT                                                      
046101                                                                          
046201     MOVE ZERO TO RETURN-CODE                                             
046301     GOBACK                                                               
046401     .                                                                    
046501     EJECT                                                                
046601 A-INIT SECTION.                                                          
046701     MOVE 'A-INIT            '      TO WS-SEKTION                         
046801                                                                          
046901     OPEN INPUT  W476IN                                                   
047001     OPEN OUTPUT W47680                                                   
047101                                                                          
047201     MOVE NEJ          TO INFIL-EOF                                       
047301                                                                          
047401     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
047501                                                                          
047601     MOVE ZERO         TO WS-SUORDV-FAKT                                  
047701                          WS-VKRAD-NTO-TOT                                
047801                          WS-VKORDBTO-TOT                                 
047901                          WS-KVFAKTUR                                     
048001                          WS-CUST-CNT                                     
048101                          ROW-IX                                          
048201                          CUST-IX                                         
048301                          CUST-PRICE-TABLE                                
048401     MOVE SPACE        TO WS-IDSIGILL                                     
048501                                                                          
048601     MOVE NEJ          TO OLD-INVOICE-SW                                  
048701     .                                                                    
048801     EJECT                                                                
048901 B-LAES-PARAMETER SECTION.                                                
049001     MOVE 'B-LAES-PARAMETER  '      TO WS-SEKTION                         
049101                                                                          
049201     READ W476IN INTO IN-AREA                                             
049301     END-READ                                                             
049401                                                                          
049501     MOVE IN-IDTULL     TO W-IDTULL-MIN                                   
049601                           W-IDTULL-MAX                                   
049701     DISPLAY 'TULL ID.= ' IN-IDTULL                                       
049801     .                                                                    
049901     EJECT                                                                
050001 B-SKRIV-RADFIL  SECTION.                                                 
050101     MOVE 'B-SKRIV-RADFIL    '   TO WS-SEKTION                            
050201                                                                          
050301     MOVE HUV-IDFAKT             TO W-IDFAKT-MIN                          
050401                                    W-IDFAKT-MAX                          
050501     MOVE HUV-IDPRODNR           TO W-IDPRODNR-MIN                        
050601                                    W-IDPRODNR-MAX                        
050701                                    W-IDPRODNR                            
050801     MOVE HUV-IDKOLLI            TO W-IDKOLLI-MIN                         
050901                                    W-IDKOLLI-MAX                         
051001                                    W-IDKOLLI                             
051101                                                                          
051201     MOVE HUV-IDDC               TO UT-TU1-IDDC                           
051301                                                                          
051401     PERFORM IMS-GU-WDE611                                                
051501                                                                          
051601     PERFORM IMS-GU-WDM801                                                
051701                                                                          
051801     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
051901                                                                          
052001       MOVE 'TU2'                TO UT-TU2-IDPTYP                         
052101       MOVE HUV-IDFAKT           TO UT-TU2-IDFAKT                         
052201       MOVE IN-IDTULL            TO UT-TU2-IDTULL                         
052301       MOVE RAD-IDSTATNR         TO UT-TU2-IDSTATNR                       
052401       MOVE RAD-IDPRODNR         TO UT-TU2-IDPRODNR                       
052501       MOVE RAD-IDKOLLI          TO UT-TU2-IDKOLLI                        
052601       MOVE RAD-IDARTNR          TO UT-TU2-IDARTNR                        
052701       MOVE RAD-KDARTURS         TO UT-TU2-KDARTURS                       
052801       MOVE RAD-KVLEVART         TO UT-TU2-KVLEVART                       
052901       MOVE RAD-KDVALISO         TO UT-TU2-KDVALISO                       
053001       MOVE RAD-KDVALISO-AVC     TO UT-TU2-KDVALISO-AVC                   
053101       IF RAD-KDVALISO-AVC > SPACE                                        
053201         MOVE RAD-KDVALISO-AVC   TO WS-KDVALISO                           
053301       ELSE                                                               
053401         MOVE RAD-KDVALISO       TO WS-KDVALISO                           
053501       END-IF                                                             
053601                                                                          
053701       MOVE SPACE                TO UT-TU2-IDKOLLI-MIC                    
053801                                    UT-TU2-KDSORT                         
053901       MOVE ZERO                 TO UT-TU2-KDEMBTYP                       
054001                                                                          
054101       MOVE RAD-PRARTNTO         TO UT-TU2-PRARTNTO                       
054201       MOVE RAD-PRAVCOST         TO UT-TU2-PRAVCOST                       
054301                                                                          
054401       IF RAD-PRAVCOST > ZERO                                             
054501         COMPUTE UT-TU2-SUARTNTO = RAD-KVLEVART * RAD-PRAVCOST            
054601       ELSE                                                               
054701         COMPUTE UT-TU2-SUARTNTO = RAD-KVLEVART * RAD-PRARTNTO            
054801       END-IF                                                             
054901                                                                          
055001       COMPUTE UT-TU2-VKRAD-NTO-KG =                                      
055101                      RAD-KVLEVART * RAD-VKART-NTO-KG                     
055201                                                                          
055301       COMPUTE WS-SUORDV-FAKT  = WS-SUORDV-FAKT  + UT-TU2-SUARTNTO        
055401                                                                          
055501       COMPUTE WS-VKRAD-NTO-TOT =                                         
055601                      WS-VKRAD-NTO-TOT + UT-TU2-VKRAD-NTO-KG              
055701                                                                          
055801       IF SEGMENT-FINNS-E6                                                
055901         MOVE KOLLI-VKORDBTO-KOLLI TO UT-TU2-VKORDBTO-KOLLI               
056001         MOVE KOLLI-VLORDBTO-KOLLI TO UT-TU2-VLORDBTO-KOLLI               
056101       ELSE                                                               
056201         MOVE ZERO                 TO UT-TU2-VKORDBTO-KOLLI               
056301                                      UT-TU2-VLORDBTO-KOLLI               
056401       END-IF                                                             
056501                                                                          
056601*--    I FILEN TILL MIC ANVÄNTS DEN NYA VIKTEN, UTAN ART.EMBALLAGE        
056701*--    VKART-NTO-KG                                                       
056801*--                                                                       
056901*--    DATA TILL NYA FILEN TILL FLS/MIC                                   
057001                                                                          
057101       MOVE HUV-IDDISTR         TO TEST-IDDISTR                           
057201       IF DIST42-EJ-EU-MIC OR                                             
057301          DIST42-OVR-MIC                                                  
057401                                                                          
057501         MOVE HUV-IDDISTR       TO WS-IDDISTR                             
057601         MOVE HUV-IDKUNDNR      TO WS-IDKUNDNR                            
057701         MOVE HUV-IDORDNR7      TO WS-IDORDNR7                            
057801         MOVE HUV-IDKOLLI       TO WS-IDKOLLI-M                           
057901         MOVE WS-IDKOLLI-MIC    TO UT-TU2-IDKOLLI-MIC                     
058001                                                                          
058101         IF HUV-KDEMBTYP > ZERO                                           
058201           MOVE HUV-KDEMBTYP    TO UT-TU2-KDEMBTYP                        
058301         ELSE                                                             
058401                                                                          
058501           IF SEGMENT-FINNS-E6                                            
058601             MOVE KOLLI-KDEMBTYP TO UT-TU2-KDEMBTYP                       
058701           ELSE                                                           
058801             MOVE 1             TO UT-TU2-KDEMBTYP                        
058901           END-IF                                                         
059001         END-IF                                                           
059101                                                                          
059201*--      LÄS WDK6 FÖR ATT TA REDA PÅ ART-KDSORT                           
059301         MOVE RAD-IDARTNR       TO W-IDARTNR                              
059401         PERFORM IMS-GU-WDK601                                            
059501         IF SEGMENT-FINNS-K6                                              
059601           MOVE ART-KDSORT      TO UT-TU2-KDSORT                          
059701         ELSE                                                             
059801           MOVE 'ST'            TO UT-TU2-KDSORT                          
059901         END-IF                                                           
060001                                                                          
060101         MOVE GMT-IDPARTNER     TO UT-TU2-IDPARTNER                       
060201       END-IF                                                             
060301                                                                          
060401       MOVE RAD-FLPCOO          TO UT-TU2-FLPCOO                          
060501*EUDR FIELDS                                                              
060601       MOVE RAD-IDREFDDS        TO UT-TU2-IDREFDDS                        
060701       MOVE RAD-KDORSAK         TO UT-TU2-KDORSAK                         
060801                                                                          
060901       PERFORM BA-GET-IDPSN                                               
061001                                                                          
061101       WRITE UT-RAD FROM ARB-RAD-UTAREA                                   
061201       MOVE UT-TU2-IDPTYP TO POSTSUM-TRANSTYP                             
061301       MOVE 'W47680' TO POSTSUM-FDNAMN                                    
061401       MOVE 'W47680D2' TO POSTSUM-DDNAMN2                                 
061501       CALL POSTSUM USING POSTSUM-PARM                                    
061601                                                                          
061701       PERFORM IMS-GN-WDM801                                              
061801                                                                          
061901     END-PERFORM                                                          
062001     .                                                                    
062101     EJECT                                                                
062201*****************************************************************         
062301***GET THE IDPSN FOR THE PART IN WDK611                         *         
062401*****************************************************************         
062501 BA-GET-IDPSN  SECTION.                                                   
062601     MOVE 'BA-GET-IDPSN      '   TO WS-SEKTION                            
062701                                                                          
062801     MOVE ZERO                   TO W-IDPSN                               
062901     MOVE RAD-IDARTNR            TO W-IDARTNR                             
063001                                                                          
063101     PERFORM IMS-GU-WDK611                                                
063201                                                                          
063301     IF SEGMENT-FINNS                                                     
063401        IF CLAG-IDPSN = 0                                                 
063501           MOVE ZERO             TO W-IDPSN                               
063601           MOVE SPACE            TO UT-TU2-BEPSNUN                        
063701        ELSE                                                              
063801           MOVE CLAG-IDPSN       TO W-IDPSN                               
063901           PERFORM BAA-CHECK-PSN-VALIDITY                                 
064001           IF SEND-UN-NUM                                                 
064101              PERFORM BAB-GET-UN-NUM                                      
064201           ELSE                                                           
064301              MOVE ZERO          TO W-IDPSN                               
064401              MOVE SPACE         TO UT-TU2-BEPSNUN                        
064501           END-IF                                                         
064601        END-IF                                                            
064701     ELSE                                                                 
064801        MOVE ZERO                TO W-IDPSN                               
064901        MOVE SPACE               TO UT-TU2-BEPSNUN                        
065001     END-IF                                                               
065101     .                                                                    
065201     EJECT                                                                
065301*****************************************************************         
065401***CHECK IF PSN IS VALID BASED ON TRANSPORT TYPE                *         
065501***HUV-KDTRPTYP = 1  (BOAT)        CHECK GOOD-PSN-IMDG          *         
065601***HUV-KDTRPTYP = 2/3(TRAIN/TRUCK) CHECK GOOD-PSN-ADR           *         
065701***HUV-KDTRPTYP = 4 (AIR )         CHECK GOOD-PSN-DGR           *         
065801*****************************************************************         
065901 BAA-CHECK-PSN-VALIDITY SECTION.                                          
066001     MOVE 'BAA-CHECK-PSN-    '   TO WS-SEKTION                            
066101                                                                          
066201     MOVE NEJ                    TO SEND-UN-NUM-SW                        
066301     MOVE ZEROES                 TO GOOD-PSN-ADR-SW                       
066401                                    GOOD-PSN-DGR-SW                       
066501                                    GOOD-PSN-IMDG-SW                      
066601                                                                          
066701     IF HUV-KDTRPTYP = 1                                                  
066801        MOVE W-IDPSN             TO GOOD-PSN-IMDG-SW                      
066901        IF GOOD-PSN-IMDG                                                  
067001           SET SEND-UN-NUM       TO TRUE                                  
067101        END-IF                                                            
067201     ELSE                                                                 
067301        IF HUV-KDTRPTYP = 2 OR HUV-KDTRPTYP = 3                           
067401          MOVE W-IDPSN           TO GOOD-PSN-ADR-SW                       
067501          IF GOOD-PSN-ADR                                                 
067601             SET SEND-UN-NUM     TO TRUE                                  
067701          END-IF                                                          
067801        ELSE                                                              
067901           IF HUV-KDTRPTYP = 4                                            
068001             MOVE W-IDPSN        TO GOOD-PSN-DGR-SW                       
068101             IF GOOD-PSN-DGR                                              
068201                SET SEND-UN-NUM  TO TRUE                                  
068301             END-IF                                                       
068401           END-IF                                                         
068501        END-IF                                                            
068601     END-IF                                                               
068701     .                                                                    
068801     EJECT                                                                
068901*****************************************************************         
069001***GET ENGLISH TEXT FOR IDPSN FOR CORRESSPONDING TRANSPORT      *         
069101*****************************************************************         
069201 BAB-GET-UN-NUM    SECTION.                                               
069301     MOVE 'BAB-GET-UN-NUM    '   TO WS-SEKTION                            
069401                                                                          
069501     PERFORM BABA-MATCH-PSN-WDR2                                          
069601                                                                          
069701     MOVE SPACES                 TO WS-PSN-NAME                           
069801     PERFORM IMS-GET-1165-WDR2                                            
069901                                                                          
070001     IF SEGMENT-FINNS                                                     
070101        MOVE 1168-BEPSN(1)       TO WS-PSN-NAME                           
070201        IF WS-PSN-NAME(1:2) = 'UN'                                        
070301           MOVE WS-PSN-NAME(1:6) TO UT-TU2-BEPSNUN                        
070401        ELSE                                                              
070501           MOVE SPACES           TO UT-TU2-BEPSNUN                        
070601        END-IF                                                            
070701     ELSE                                                                 
070801        MOVE SPACES              TO UT-TU2-BEPSNUN                        
070901     END-IF                                                               
071001     .                                                                    
071101     EJECT                                                                
071201                                                                          
071301*****************************************************************         
071401***MAP THE PSN IN WDM7 DB AND WDR2 DB                           *         
071501* ***************WDM7*******WDR2*********************************         
071601*              KDTRPTYP    KDFGTRP                              *         
071701* BOAT       *    1     *    02                                 *         
071801* TRAIN      *    2     *    04                                 *         
071901* TRUCK/CAR  *    3     *    04                                 *         
072001* AIR        *    4     *    01                                 *         
072101*****************************************************************         
072201 BABA-MATCH-PSN-WDR2 SECTION.                                             
072301     MOVE 'BABA-MATCH-PSN-   '   TO WS-SEKTION                            
072401                                                                          
072501     IF HUV-KDTRPTYP = 1                                                  
072601        MOVE 02                  TO W-KDFGTRP                             
072701     ELSE                                                                 
072801        IF HUV-KDTRPTYP = 4                                               
072901           MOVE 01               TO W-KDFGTRP                             
073001        ELSE                                                              
073101           IF HUV-KDTRPTYP = 2 OR HUV-KDTRPTYP = 3                        
073201              MOVE 04            TO W-KDFGTRP                             
073301           END-IF                                                         
073401        END-IF                                                            
073501     END-IF                                                               
073601     .                                                                    
073701     EJECT                                                                
073801 C-SKAPA-KOLLI-TOT SECTION.                                               
073901     MOVE 'C-SKAPA-KOLLI-TOT '   TO WS-SEKTION                            
074001                                                                          
074101     MOVE HUV-IDDISTR TO TEST-IDDISTR                                     
074201     IF DIST35-ST-CDC                                                     
074301       MOVE HUV-IDKOLLI TO WS-IDKOLLI                                     
074401       IF WS-IDKOLLI-POS3 = 9                                             
074501         ADD WS-IDKOLLI-POS5 TO WS-KVKOLLI-TULL                           
074601       ELSE                                                               
074701         ADD +1                   TO WS-KVKOLLI-TULL                      
074801         IF DIST42-EJ-EU-MIC OR                                           
074901            DIST42-OVR-MIC                                                
075001           IF HUV-VKORDBTO-KOLLI > ZERO                                   
075101             COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                  
075201                                       HUV-VKORDBTO-KOLLI                 
075301           ELSE                                                           
075401             IF SEGMENT-FINNS-E6                                          
075501               COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                
075601                                         KOLLI-VKORDBTO-KOLLI             
075701             ELSE                                                         
075801               ADD 1            TO WS-VKORDBTO-TOT                        
075901             END-IF                                                       
076001           END-IF                                                         
076101         ELSE                                                             
076201           MOVE ZERO              TO WS-VKORDBTO-TOT                      
076301         END-IF                                                           
076401       END-IF                                                             
076501     ELSE                                                                 
076601       ADD +1                     TO WS-KVKOLLI-TULL                      
076701       IF DIST42-EJ-EU-MIC OR                                             
076801          DIST42-OVR-MIC                                                  
076901         IF HUV-VKORDBTO-KOLLI > ZERO                                     
077001           COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                    
077101                                     HUV-VKORDBTO-KOLLI                   
077201         ELSE                                                             
077301           IF SEGMENT-FINNS-E6                                            
077401             COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                  
077501                                       KOLLI-VKORDBTO-KOLLI               
077601           ELSE                                                           
077701             ADD 1                TO WS-VKORDBTO-TOT                      
077801           END-IF                                                         
077901         END-IF                                                           
078001       ELSE                                                               
078101         MOVE ZERO                TO WS-VKORDBTO-TOT                      
078201       END-IF                                                             
078301     END-IF                                                               
078401     .                                                                    
078501     EJECT                                                                
078601*****************************************************************         
078701*STORE THE FIRST CUST,PRICES IN 'CUST-PRICE-TABLE'.             *         
078801*FOR THE NEXT CUST,CHECK IF IT EXISTS IN TEMP TABLE.IF EXISTS,  *         
078901*SKIP ENTRY IN TEMP TABLE.IF NOT, ENTER REC INTO TEMP TABLE.    *         
079001*****************************************************************         
079101 D-SAVE-ADD-COSTS-DIFF-CUST SECTION.                                      
079201     MOVE 'D-SAVE-ADD-COSTS  '   TO WS-SEKTION                            
079301                                                                          
079401     MOVE 'N'                 TO CUST-FND-SW                              
079501                                                                          
079601     IF CUST-IX = 1                                                       
079701        MOVE 1                TO WS-CUST-CNT                              
079801        MOVE HUV-IDKUNDNR     TO TEMP-IDKUNDNR(CUST-IX)                   
079901        MOVE HUV-PREMBHNT     TO TEMP-PREMBHNT(CUST-IX)                   
080001        MOVE HUV-PRFRAKT      TO TEMP-PRFRAKT (CUST-IX)                   
080101        MOVE HUV-PRFOERS      TO TEMP-PRFOERS (CUST-IX)                   
080201        MOVE HUV-PRLEGKST     TO TEMP-PRLEGKST(CUST-IX)                   
080301     ELSE                                                                 
080401        MOVE CUST-IX          TO   ROW-IX                                 
080501        SUBTRACT +1           FROM ROW-IX                                 
080601        PERFORM UNTIL CUST-FND OR ROW-IX < 1                              
080701           IF HUV-IDKUNDNR = TEMP-IDKUNDNR(ROW-IX)                        
080801               SET CUST-FND   TO TRUE                                     
080901               SUBTRACT +1           FROM CUST-IX                         
081001           END-IF                                                         
081101           COMPUTE ROW-IX = ROW-IX - 1                                    
081201        END-PERFORM                                                       
081301        IF NOT CUST-FND                                                   
081401           ADD +1             TO WS-CUST-CNT                              
081501           MOVE HUV-IDKUNDNR  TO TEMP-IDKUNDNR(CUST-IX)                   
081601           MOVE HUV-PREMBHNT  TO TEMP-PREMBHNT(CUST-IX)                   
081701           MOVE HUV-PRFRAKT   TO TEMP-PRFRAKT (CUST-IX)                   
081801           MOVE HUV-PRFOERS   TO TEMP-PRFOERS (CUST-IX)                   
081901           MOVE HUV-PRLEGKST  TO TEMP-PRLEGKST(CUST-IX)                   
082001        END-IF                                                            
082101     END-IF                                                               
082201     ADD +1                    TO CUST-IX                                 
082301     .                                                                    
082401     EJECT                                                                
082501 E-SKRIV-FAKT-HUV SECTION.                                                
082601     MOVE 'E-SKRIV-FAKT-HUV  '   TO WS-SEKTION                            
082701                                                                          
082801     IF WS-KVFAKTUR > ZERO                                                
082901       MOVE NEJ                TO UT-TU1-FLSLUT                           
083001     END-IF                                                               
083101                                                                          
083201     MOVE WS-KVFAKTUR          TO UT-TU1-KVFAKTUR                         
083301     MOVE WS-KVKOLLI-TULL      TO UT-TU1-KVKOLLI-FAKT                     
083401     MOVE WS-SUORDV-FAKT       TO UT-TU1-SUORDV-FAKT                      
083501     MOVE WS-VKRAD-NTO-TOT     TO UT-TU1-VKRAD-NTO-KG-FAKT                
083601     MOVE WS-VKORDBTO-TOT      TO UT-TU1-VKORDBTO-FAKT                    
083701     MOVE WS-KDVALISO          TO UT-TU1-KDVALISO                         
083801                                                                          
083901     WRITE UT-HUV FROM ARB-HUV-UTAREA                                     
084001     MOVE UT-TU1-IDPTYP TO POSTSUM-TRANSTYP                               
084101                                                                          
084201     MOVE 'W47680' TO POSTSUM-FDNAMN                                      
084301     MOVE 'W47680D2' TO POSTSUM-DDNAMN2                                   
084401     CALL POSTSUM USING POSTSUM-PARM                                      
084501                                                                          
084601     MOVE ZERO                 TO WS-SUORDV-FAKT                          
084701                                  WS-VKRAD-NTO-TOT                        
084801                                  WS-VKORDBTO-TOT                         
084901                                  WS-KVKOLLI-TULL                         
085001     .                                                                    
085101     EJECT                                                                
085201 F-KOLLA-OM-FLER-FAKT SECTION.                                            
085301     MOVE 'F-KOLLA-OM-FLER-F.'   TO WS-SEKTION                            
085401                                                                          
085501     IF SEGMENT-FINNS                                                     
085601       IF HUV-IDFAKT = SPAR-IDFAKT                                        
085701         CONTINUE                                                         
085801       ELSE                                                               
085901         ADD +1 TO WS-KVFAKTUR                                            
086001       END-IF                                                             
086101     END-IF                                                               
086201     .                                                                    
086301     EJECT                                                                
086401 G-SKRIV-SISTA-FAKT-HUV  SECTION.                                         
086501     MOVE 'G-SKRIV-SISTA-FAKT'   TO WS-SEKTION                            
086601                                                                          
086701     MOVE 'TU1'                TO UT-TU1-IDPTYP                           
086801                                                                          
086901     MOVE IN-IDTULL            TO UT-TU1-IDTULL                           
087001     MOVE HUV-IDFAKT           TO UT-TU1-IDFAKT                           
087101     MOVE HUV-TIFAKT           TO UT-TU1-TIFAKT                           
087201     MOVE HUV-IDUSER           TO UT-TU1-IDUSER                           
087301     MOVE WS-KVFAKTUR          TO UT-TU1-KVFAKTUR                         
087401     MOVE HUV-IDDISTR          TO UT-TU1-IDDISTR                          
087501                                  W-IDDISTR                               
087601                                  W-IDDISTR-DEF                           
087701     MOVE HUV-IDKUNDNR         TO UT-TU1-IDKUNDNR                         
087801                                  W-IDKUNDNR                              
087901     MOVE HUV-KDFRAKT          TO UT-TU1-KDFRAKT                          
088001                                                                          
088101     MOVE HUV-IDPRODNR         TO W-IDPRODNR                              
088201     PERFORM IMS-GU-WDE601                                                
088301     IF SEGMENT-FINNS                                                     
088401       MOVE VORD-KDFRAKT       TO UT-TU1-KDFRAKT                          
088501       MOVE VORD-DARFS         TO UT-TU1-DARFS                            
088601     END-IF                                                               
088701                                                                          
088801     MOVE UT-TU1-KDFRAKT       TO W-KDFRAKT                               
088901                                  W-KDFRAKT-DEF                           
089001     IF HUV-IDDC               =  WC-CDC-SE OR                            
089101                                  WC-DDC-SE                               
089201       MOVE WC-CDC-SE          TO W-IDDC                                  
089301     ELSE                                                                 
089401       MOVE HUV-IDDC           TO WC-DDC-SE                               
089501                                  W-IDDC-DEF                              
089601     END-IF                                                               
089701                                                                          
089801     PERFORM IMS-GU-WDB501                                                
089901     IF SEGMENT-SAKNAS OR FK-KDGRANS = ZERO                               
090001       IF HUV-IDDC             =  WC-CDC-SE OR                            
090101                                  WC-DDC-SE                               
090201         IF UT-TU1-KDFRAKT = +14 OR +16 OR +17 OR                         
090301                             +18 OR +19 OR +50                            
090401           MOVE +609           TO UT-TU1-KDGRANS                          
090501         ELSE                                                             
090601           MOVE +599           TO UT-TU1-KDGRANS                          
090701         END-IF                                                           
090801       END-IF                                                             
090901     ELSE                                                                 
091001       MOVE FK-KDGRANS         TO UT-TU1-KDGRANS                          
091101     END-IF                                                               
091201                                                                          
091301     MOVE HUV-ADKOPARE-RAD1    TO UT-TU1-ADKOPARE-RAD1                    
091401     MOVE HUV-ADKOPARE-RAD2    TO UT-TU1-ADKOPARE-RAD2                    
091501     MOVE HUV-BEKOPARE-RAD1    TO UT-TU1-BEKOPARE-RAD1                    
091601     MOVE HUV-BEKOPARE-RAD2    TO UT-TU1-BEKOPARE-RAD2                    
091701     MOVE WS-KVKOLLI-TULL      TO UT-TU1-KVKOLLI-FAKT                     
091801     MOVE WS-SUORDV-FAKT       TO UT-TU1-SUORDV-FAKT                      
091901     MOVE WS-VKRAD-NTO-TOT     TO UT-TU1-VKRAD-NTO-KG-FAKT                
092001     MOVE WS-VKORDBTO-TOT      TO UT-TU1-VKORDBTO-FAKT                    
092101     MOVE WS-KDVALISO          TO UT-TU1-KDVALISO                         
092201     MOVE HUV-KDORDKL          TO UT-TU1-KDORDKL                          
092301     MOVE HUV-IDLBBET          TO UT-TU1-IDLBBET                          
092401     MOVE HUV-IDBOKN           TO UT-TU1-IDBOKN                           
092501     MOVE SPACE                TO UT-TU1-IDPARTNR                         
092601                                  UT-TU1-BELEVVIL                         
092701                                                                          
092801     IF WS-KVFAKTUR > ZERO                                                
092901       ADD +1                  TO UT-TU1-KVFAKTUR                         
093001       MOVE JA                 TO UT-TU1-FLSLUT                           
093101     END-IF                                                               
093201                                                                          
093301*--  NYA FILEN TILL FLS/MIC                                               
093401*    HÄMTA LEVERANSVILLKOR, PARMANR.OCH PARMAKODEN                        
093501                                                                          
093601     MOVE HUV-IDDISTR          TO TEST-IDDISTR                            
093701     IF DIST42-EJ-EU-MIC OR                                               
093801        DIST42-OVR-MIC                                                    
093901                                                                          
094001       MOVE HUV-IDDISTR        TO W-IDDISTR-WDB2                          
094101       MOVE HUV-IDKUNDNR       TO W-IDKUNDNR-WDB2                         
094201       PERFORM IMS-GU-WDB201                                              
094301*                                                                         
094401       IF HUV-IDPARTNR > SPACE                                            
094501         MOVE HUV-IDPARTNR     TO UT-TU1-IDPARTNR                         
094601       ELSE                                                               
094701         MOVE HUV-IDSKEPPN     TO W-IDSHIPM                               
094801         MOVE HUV-IDDISTR      TO W-IDDISTR-E2                            
094901         MOVE HUV-IDKUNDNR     TO W-IDKUNDNR-E2                           
095001         PERFORM IMS-GU-WDE211                                            
095101         IF SEGMENT-FINNS                                                 
095201           MOVE BGMT-IDPARTNR  TO UT-TU1-IDPARTNR                         
095301           IF HUV-BELEVVIL NOT > SPACE                                    
095401             MOVE JA           TO WDE211-SW                               
095501           END-IF                                                         
095601         ELSE                                                             
095701           MOVE NEJ            TO WDE211-SW                               
095801           MOVE GMT-IDPARTNR   TO UT-TU1-IDPARTNR                         
095901         END-IF                                                           
096001       END-IF                                                             
096101                                                                          
096201       IF HUV-BELEVVIL > SPACE                                            
096301         MOVE HUV-BELEVVIL     TO UT-TU1-BELEVVIL                         
096401       ELSE                                                               
096501         IF WDE211-OK                                                     
096601           CONTINUE                                                       
096701*          HAR REDAN LÄST IN SEG. LITE LÄNGRE UPP                         
096801         ELSE                                                             
096901           MOVE HUV-IDSKEPPN   TO W-IDSHIPM                               
097001           MOVE HUV-IDDISTR    TO W-IDDISTR-E2                            
097101                                  TEST-IDDISTR                            
097201           MOVE HUV-IDKUNDNR   TO W-IDKUNDNR-E2                           
097301           PERFORM IMS-GU-WDE211                                          
097401           IF SEGMENT-FINNS                                               
097501             MOVE JA           TO WDE211-SW                               
097601           END-IF                                                         
097701         END-IF                                                           
097801         IF WDE211-OK                                                     
097901                                                                          
098001           IF BGMT-KDLEVVIL > ZERO                                        
098101             MOVE BGMT-KDLEVVIL TO KDLEVVIL-4735                          
098201*                                                                         
098301             PERFORM IMS-4735-GET-ROOT                                    
098401             IF SEGMENT-FINNS                                             
098501               PERFORM IMS-4735-GET-SEGMENT                               
098601               IF SEGMENT-FINNS                                           
098701                 MOVE LEVVIL-BELEVVIL(2) TO UT-TU1-BELEVVIL               
098801               ELSE                                                       
098901                 MOVE SPACE    TO UT-TU1-BELEVVIL                         
099001               END-IF                                                     
099101             ELSE                                                         
099201               MOVE SPACE      TO UT-TU1-BELEVVIL                         
099301             END-IF                                                       
099401           ELSE                                                           
099501             PERFORM GA-SPEC-LEVVIL                                       
099601           END-IF                                                         
099701*                                                                         
099801         ELSE                                                             
099901           IF DIST41-CIP-FRAKT                                            
100001             MOVE 'CIP                            '                       
100101                               TO UT-TU1-BELEVVIL                         
100201           ELSE                                                           
100301             MOVE 'CIP            (INCOTERMS 2010)'                       
100401                               TO UT-TU1-BELEVVIL                         
100501           END-IF                                                         
100601                                                                          
100701         END-IF                                                           
100801       END-IF                                                             
100901     END-IF                                                               
101001                                                                          
101101*ADD THE ADDITIONAL COSTS IN THE TEMP TABLE                               
101201     IF OLD-INVOICE-SW = NEJ                                              
101301        MOVE +1                   TO CUST-IX                              
101401        PERFORM UNTIL CUST-IX > WS-CUST-CNT      OR                       
101501                      CUST-IX > MAX-CUST                                  
101601          COMPUTE UT-TU1-PREMBHNT = UT-TU1-PREMBHNT   +                   
101701                                    TEMP-PREMBHNT(CUST-IX)                
101801          COMPUTE UT-TU1-PRFRAKT  = UT-TU1-PRFRAKT    +                   
101901                                    TEMP-PRFRAKT(CUST-IX)                 
102001          COMPUTE UT-TU1-PRFOERS  = UT-TU1-PRFOERS    +                   
102101                                    TEMP-PRFOERS(CUST-IX)                 
102201          COMPUTE UT-TU1-PRLEGKST = UT-TU1-PRLEGKST   +                   
102301                                    TEMP-PRLEGKST(CUST-IX)                
102401          ADD +1                  TO CUST-IX                              
102501        END-PERFORM                                                       
102601     END-IF                                                               
102701*WRITE IDSIGILL                                                           
102801     MOVE WS-IDSIGILL             TO UT-TU1-IDSIGILL                      
102901                                                                          
103001     WRITE UT-HUV FROM ARB-HUV-UTAREA                                     
103101     MOVE UT-TU1-IDPTYP TO POSTSUM-TRANSTYP                               
103201                                                                          
103301     MOVE 'W47680' TO POSTSUM-FDNAMN                                      
103401     MOVE 'W47680D2' TO POSTSUM-DDNAMN2                                   
103501     CALL POSTSUM USING POSTSUM-PARM                                      
103601     .                                                                    
103701     EJECT                                                                
103801 GA-SPEC-LEVVIL SECTION.                                                  
103901     MOVE 'G-SPEC-LEVVIL     '   TO WS-SEKTION                            
104001                                                                          
104101     IF DIST41-DDU-FRAKT                                                  
104201       MOVE       'DDU CONSIGNEE (INCOTERMS 2010) '       TO              
104301                 UT-TU1-BELEVVIL                                          
104401     ELSE                                                                 
104501       IF DIST41-CIP-FRAKT                                                
104601         MOVE     'CIP                            '       TO              
104701                 UT-TU1-BELEVVIL                                          
104702         IF DIST25-ISRAEL-FRAKT AND                                       
104703            HUV-KDORDKL  = 1    AND                                       
104704            HUV-KDFRAKT  = 17                                             
104705            MOVE  'FCA                            '  TO                   
104706                 UT-TU1-BELEVVIL                                          
104707         END-IF                                                           
104801       ELSE                                                               
104901         MOVE BGMT-IDDISTR TO TEST-IDDISTR                                
105001         IF BGMT-PRFRAKT = ZERO                                           
105101           EVALUATE TRUE                                                  
105201           WHEN DIST76-ROMANIA                                            
105301             MOVE 'CIP CONSIGNEE                  '       TO              
105401                   UT-TU1-BELEVVIL                                        
105501           WHEN DIST76-RYSSLAND                                           
105601             MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '       TO              
105701                   UT-TU1-BELEVVIL                                        
105801           WHEN DIST76-RYSSLAND-2606                                      
105901             MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO                
106001                   UT-TU1-BELEVVIL                                        
106101           WHEN DIST76-VITRYSSLAND                                        
106201             MOVE 'CIP DNEPROPETROVSK             '       TO              
106301                   UT-TU1-BELEVVIL                                        
106401           WHEN DIST76-PERU                                               
106501             MOVE 'FCA GOTHENBURG                 '       TO              
106601                   UT-TU1-BELEVVIL                                        
106701           WHEN DIST76-BELARUS                                            
106801             MOVE 'CIP                            '       TO              
106901                   UT-TU1-BELEVVIL                                        
107001           WHEN DIST76-MOLDAVIA                                           
107101             MOVE 'DAP CHISINAU                   '       TO              
107201                   UT-TU1-BELEVVIL                                        
107301           WHEN DIST76-OMAN                                               
107401             MOVE 'DDP                            '       TO              
107501                   UT-TU1-BELEVVIL                                        
107601           WHEN DIST76-COLUMBIA                                           
107701             MOVE 'FCA GOTHENBURG                 '       TO              
107801                   UT-TU1-BELEVVIL                                        
107901           WHEN DIST76-CANADA-REF                                         
108001             MOVE 'CIP                            '       TO              
108101                   UT-TU1-BELEVVIL                                        
108201           WHEN DIST76-SLOVENIA                                           
108301             MOVE 'CIP DEALER                     '       TO              
108401                   UT-TU1-BELEVVIL                                        
108501           WHEN DIST76-BOSNIA                                             
108601             MOVE 'CIP LJUBLJANA                  '       TO              
108701                   UT-TU1-BELEVVIL                                        
108801           WHEN DIST76-MACEDONIA                                          
108901             MOVE 'CIP LJUBLJANA                  '       TO              
109001                   UT-TU1-BELEVVIL                                        
109101           WHEN DIST76-SERBIA                                             
109201             MOVE 'CIP LJUBLJANA                  '       TO              
109301                   UT-TU1-BELEVVIL                                        
109401           WHEN DIST76-GEORGIA                                            
109501             MOVE 'CIP TBILISI                    '       TO              
109601                   UT-TU1-BELEVVIL                                        
109701           WHEN DIST76-TUNISIA                                            
109801             MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'       TO              
109901                   UT-TU1-BELEVVIL                                        
110001           WHEN OTHER                                                     
110101             MOVE 'FCA GÖTEBORG   (INCOTERMS 2010)'       TO              
110201                   UT-TU1-BELEVVIL                                        
110301           END-EVALUATE                                                   
110401                                                                          
110501         ELSE                                                             
110601           IF BGMT-PRFOERS = ZERO                                         
110701             EVALUATE TRUE                                                
110801             WHEN DIST76-ROMANIA                                          
110901               MOVE 'CIP CONSIGNEE                  '     TO              
111001                     UT-TU1-BELEVVIL                                      
111101             WHEN DIST76-RYSSLAND                                         
111201               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
111301                     UT-TU1-BELEVVIL                                      
111401             WHEN DIST76-RYSSLAND-2606                                    
111501               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
111601                     UT-TU1-BELEVVIL                                      
111701             WHEN DIST76-VITRYSSLAND                                      
111801               MOVE 'CIP DNEPROPETROVSK             '     TO              
111901                     UT-TU1-BELEVVIL                                      
112001             WHEN DIST76-PERU                                             
112101               MOVE 'CPT                            '     TO              
112201                     UT-TU1-BELEVVIL                                      
112301             WHEN DIST76-BELARUS                                          
112401               MOVE 'CIP                            '     TO              
112501                     UT-TU1-BELEVVIL                                      
112601             WHEN DIST76-MOLDAVIA                                         
112701               MOVE 'DAP CHISINAU                   '     TO              
112801                     UT-TU1-BELEVVIL                                      
112901             WHEN DIST76-OMAN                                             
113001               MOVE 'DDP                            '     TO              
113101                     UT-TU1-BELEVVIL                                      
113201             WHEN DIST76-COLUMBIA                                         
113301               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
113401                     UT-TU1-BELEVVIL                                      
113501             WHEN DIST76-CANADA-REF                                       
113601               MOVE 'CIP                            '     TO              
113701                     UT-TU1-BELEVVIL                                      
113801             WHEN DIST76-SLOVENIA                                         
113901               MOVE 'CIP DEALER                     '     TO              
114001                     UT-TU1-BELEVVIL                                      
114101             WHEN DIST76-BOSNIA                                           
114201               MOVE 'CIP LJUBLJANA                  '     TO              
114301                     UT-TU1-BELEVVIL                                      
114401             WHEN DIST76-MACEDONIA                                        
114501               MOVE 'CIP LJUBLJANA                  '     TO              
114601                     UT-TU1-BELEVVIL                                      
114701             WHEN DIST76-SERBIA                                           
114801               MOVE 'CIP LJUBLJANA                  '     TO              
114901                     UT-TU1-BELEVVIL                                      
115001             WHEN DIST76-GEORGIA                                          
115101               MOVE 'CIP TBILISI                    '     TO              
115201                     UT-TU1-BELEVVIL                                      
115301             WHEN DIST76-TUNISIA                                          
115401               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
115501                     UT-TU1-BELEVVIL                                      
115601             WHEN OTHER                                                   
115701               MOVE 'CPT            (INCOTERMS 2010)'     TO              
115801                     UT-TU1-BELEVVIL                                      
115901             END-EVALUATE                                                 
116001           ELSE                                                           
116101             EVALUATE TRUE                                                
116201             WHEN DIST76-ROMANIA                                          
116301               MOVE 'CIP CONSIGNEE                  '     TO              
116401                     UT-TU1-BELEVVIL                                      
116501             WHEN DIST76-RYSSLAND                                         
116601               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
116701                     UT-TU1-BELEVVIL                                      
116801             WHEN DIST76-RYSSLAND-2606                                    
116901               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
117001                     UT-TU1-BELEVVIL                                      
117101             WHEN DIST76-VITRYSSLAND                                      
117201               MOVE 'CIP DNEPROPETROVSK             '     TO              
117301                     UT-TU1-BELEVVIL                                      
117401             WHEN DIST76-PERU                                             
117501               MOVE 'CIP                            '     TO              
117601                     UT-TU1-BELEVVIL                                      
117701             WHEN DIST76-BELARUS                                          
117801               MOVE 'CIP                            '     TO              
117901                     UT-TU1-BELEVVIL                                      
118001             WHEN DIST76-MOLDAVIA                                         
118101               MOVE 'DAP CHISINAU                   '     TO              
118201                     UT-TU1-BELEVVIL                                      
118301             WHEN DIST76-OMAN                                             
118401               MOVE 'DDP                            '     TO              
118501                     UT-TU1-BELEVVIL                                      
118601             WHEN DIST76-COLUMBIA                                         
118701               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
118801                     UT-TU1-BELEVVIL                                      
118901             WHEN DIST76-SLOVENIA                                         
119001               MOVE 'CIP DEALER                     '     TO              
119101                     UT-TU1-BELEVVIL                                      
119201             WHEN DIST76-BOSNIA                                           
119301               MOVE 'CIP LJUBLJANA                  '     TO              
119401                     UT-TU1-BELEVVIL                                      
119501             WHEN DIST76-MACEDONIA                                        
119601               MOVE 'CIP LJUBLJANA                  '     TO              
119701                     UT-TU1-BELEVVIL                                      
119801             WHEN DIST76-SERBIA                                           
119901               MOVE 'CIP LJUBLJANA                  '     TO              
120001                     UT-TU1-BELEVVIL                                      
120101             WHEN DIST76-GEORGIA                                          
120201               MOVE 'CIP TBILISI                    '     TO              
120301                     UT-TU1-BELEVVIL                                      
120401             WHEN DIST76-TUNISIA                                          
120501               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
120601                     UT-TU1-BELEVVIL                                      
120701             WHEN OTHER                                                   
120801               MOVE 'CIP            (INCOTERMS 2010)'     TO              
120901                     UT-TU1-BELEVVIL                                      
121001             END-EVALUATE                                                 
121101                                                                          
121201           END-IF                                                         
121301         END-IF                                                           
121401       END-IF                                                             
121501     END-IF                                                               
121601     .                                                                    
121701     EJECT                                                                
121801 S01-SPARA-HUV SECTION.                                                   
121901     MOVE 'S01-SPARA-HUV     '   TO WS-SEKTION                            
122001                                                                          
122101     MOVE HUV-IDFAKT           TO SPAR-IDFAKT                             
122201                                                                          
122301     MOVE 'TU1'                TO UT-TU1-IDPTYP                           
122401     MOVE IN-IDTULL            TO UT-TU1-IDTULL                           
122501     MOVE HUV-IDFAKT           TO UT-TU1-IDFAKT                           
122601     MOVE HUV-TIFAKT           TO UT-TU1-TIFAKT                           
122701                                                                          
122801*TEMPORARY CODE TO PROCESS INVOICES THAT WERE CREATED BEFORE              
122901*'ACCUMULATING ADDITIONAL COST FOR DIFF CUSTOMERS IN FLS/MIC FILE"        
123001*CHANGE.                                                                  
123101     IF HUV-TIFAKT <= 250907                                              
123201         MOVE JA               TO OLD-INVOICE-SW                          
123301     END-IF                                                               
123401                                                                          
123501     MOVE HUV-IDUSER           TO UT-TU1-IDUSER                           
123601     MOVE ZERO                 TO UT-TU1-KVFAKTUR                         
123701     MOVE HUV-IDDISTR          TO UT-TU1-IDDISTR                          
123801                                  W-IDDISTR                               
123901                                  W-IDDISTR-DEF                           
124001     MOVE HUV-IDKUNDNR         TO UT-TU1-IDKUNDNR                         
124101                                  W-IDKUNDNR                              
124201     MOVE HUV-KDFRAKT          TO UT-TU1-KDFRAKT                          
124301                                                                          
124401     MOVE HUV-IDPRODNR         TO W-IDPRODNR                              
124501     PERFORM IMS-GU-WDE601                                                
124601     IF SEGMENT-FINNS                                                     
124701       MOVE VORD-KDFRAKT       TO UT-TU1-KDFRAKT                          
124801       MOVE VORD-DARFS         TO UT-TU1-DARFS                            
124901     ELSE                                                                 
125001       MOVE ZERO               TO UT-TU1-DARFS                            
125101       MOVE '  '               TO STATUS-WS                               
125201     END-IF                                                               
125301                                                                          
125401     MOVE UT-TU1-KDFRAKT       TO W-KDFRAKT                               
125501                                  W-KDFRAKT-DEF                           
125601     IF HUV-IDDC               =  WC-CDC-SE OR                            
125701                                  WC-DDC-SE                               
125801       MOVE WC-CDC-SE          TO W-IDDC                                  
125901     ELSE                                                                 
126001       MOVE HUV-IDDC           TO WC-DDC-SE                               
126101                                  W-IDDC-DEF                              
126201     END-IF                                                               
126301                                                                          
126401     PERFORM IMS-GU-WDB501                                                
126501     IF SEGMENT-SAKNAS OR FK-KDGRANS = ZERO                               
126601       IF HUV-IDDC             =  WC-CDC-SE OR                            
126701                                  WC-DDC-SE                               
126801         IF UT-TU1-KDFRAKT = +14 OR +16 OR +17 OR                         
126901                             +18 OR +19 OR +50                            
127001           MOVE +609           TO UT-TU1-KDGRANS                          
127101         ELSE                                                             
127201           MOVE +599           TO UT-TU1-KDGRANS                          
127301         END-IF                                                           
127401         MOVE '  '             TO STATUS-WS                               
127501       END-IF                                                             
127601     ELSE                                                                 
127701       MOVE FK-KDGRANS         TO UT-TU1-KDGRANS                          
127801     END-IF                                                               
127901                                                                          
128001     MOVE HUV-ADKOPARE-RAD1    TO UT-TU1-ADKOPARE-RAD1                    
128101     MOVE HUV-ADKOPARE-RAD2    TO UT-TU1-ADKOPARE-RAD2                    
128201     MOVE HUV-BEKOPARE-RAD1    TO UT-TU1-BEKOPARE-RAD1                    
128301     MOVE HUV-BEKOPARE-RAD2    TO UT-TU1-BEKOPARE-RAD2                    
128401     MOVE ZERO                 TO UT-TU1-KVKOLLI-FAKT                     
128501     MOVE ZERO                 TO UT-TU1-SUORDV-FAKT                      
128601     MOVE ZERO                 TO UT-TU1-VKRAD-NTO-KG-FAKT                
128701     MOVE ZERO                 TO UT-TU1-VKORDBTO-FAKT                    
128801     MOVE ZERO                 TO UT-TU1-KDVALISO                         
128901     MOVE HUV-KDORDKL          TO UT-TU1-KDORDKL                          
129001     MOVE HUV-IDLBBET          TO UT-TU1-IDLBBET                          
129101     MOVE HUV-IDBOKN           TO UT-TU1-IDBOKN                           
129201     MOVE HUV-PRKURS           TO UT-TU1-PRKURS                           
129301     MOVE HUV-KDTRPTYP         TO UT-TU1-KDTRPTYP                         
129401     MOVE HUV-FLCONTAIN        TO UT-TU1-FLCONTAIN                        
129501                                                                          
129601     IF OLD-INVOICE                                                       
129701        MOVE HUV-PREMBHNT      TO UT-TU1-PREMBHNT                         
129801        MOVE HUV-PRFRAKT       TO UT-TU1-PRFRAKT                          
129901        MOVE HUV-PRFOERS       TO UT-TU1-PRFOERS                          
130001        MOVE HUV-PRLEGKST      TO UT-TU1-PRLEGKST                         
130101     ELSE                                                                 
130201        MOVE ZERO              TO UT-TU1-PREMBHNT                         
130301                                  UT-TU1-PRFRAKT                          
130401                                  UT-TU1-PRFOERS                          
130501                                  UT-TU1-PRLEGKST                         
130601     END-IF                                                               
130701                                                                          
130801     MOVE HUV-IDFORDREG        TO UT-TU1-IDFORDREG                        
130901                                                                          
131201     MOVE ZERO                 TO WS-SUORDV-FAKT                          
131301                                  WS-VKRAD-NTO-TOT                        
131401                                  WS-VKORDBTO-TOT                         
131501     .                                                                    
131601     EJECT                                                                
131701 Z-FINIT SECTION.                                                         
131801     MOVE 'Z-FINIT           '   TO WS-SEKTION                            
131901                                                                          
132001     CLOSE W476IN                                                         
132101           W47680                                                         
132201     SKIP2                                                                
132301     MOVE 'S' TO POSTSUM-OPKOD                                            
132401     CALL POSTSUM USING POSTSUM-PARM                                      
132501     .                                                                    
132601     EJECT                                                                
132701 IMS-GU-WDM701  SECTION.                                                  
132801     MOVE 'IMS-GU-WDM701     '      TO WS-SEKTION                         
132901                                                                          
133001     STRING 'WDM701  (WDM7BSEQ>=' W-WDM7BSEQ-MIN-X                        
133101                    '&WDM7BSEQ<=' W-WDM7BSEQ-MAX-X ')'                    
133201           DELIMITED BY SIZE INTO SSA1                                    
133301     MOVE '    '   TO GODK-STATUSKODER                                    
133401     CALL CBLTDLI USING GU WDM7-PCB DLI-IO-WDM701 SSA1                    
133501     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
133601     PERFORM IMS-STATUSKONTROLL                                           
133701     .                                                                    
133801     SKIP2                                                                
133901 IMS-GN-WDM701  SECTION.                                                  
134001     MOVE 'IMS-GN-WDM701     '      TO WS-SEKTION                         
134101                                                                          
134201     STRING 'WDM701  (WDM7BSEQ>=' W-WDM7BSEQ-MIN-X                        
134301                    '&WDM7BSEQ<=' W-WDM7BSEQ-MAX-X ')'                    
134401           DELIMITED BY SIZE INTO SSA1                                    
134501     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
134601     CALL CBLTDLI USING GN WDM7-PCB DLI-IO-WDM701 SSA1                    
134701     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
134801     PERFORM IMS-STATUSKONTROLL                                           
134901     .                                                                    
135001     EJECT                                                                
135101 IMS-GU-WDM801  SECTION.                                                  
135201     MOVE 'IMS-GU-WDM801     '      TO WS-SEKTION                         
135301                                                                          
135401     STRING 'WDM801  (WDM801KY>=' W-WDM801KY-MIN-X                        
135501                    '&WDM801KY<=' W-WDM801KY-MAX-X ')'                    
135601           DELIMITED BY SIZE INTO SSA1                                    
135701     MOVE '  GE'   TO GODK-STATUSKODER                                    
135801     CALL CBLTDLI USING GU WDM8-PCB DLI-IO-WDM801 SSA1                    
135901     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
136001     PERFORM IMS-STATUSKONTROLL                                           
136101     .                                                                    
136201     SKIP3                                                                
136301 IMS-GN-WDM801  SECTION.                                                  
136401     MOVE 'IMS-GN-WDM801     '      TO WS-SEKTION                         
136501                                                                          
136601     STRING 'WDM801  (WDM801KY>=' W-WDM801KY-MIN-X                        
136701                    '&WDM801KY<=' W-WDM801KY-MAX-X ')'                    
136801           DELIMITED BY SIZE INTO SSA1                                    
136901     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
137001     CALL CBLTDLI USING GN WDM8-PCB DLI-IO-WDM801 SSA1                    
137101     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
137201     PERFORM IMS-STATUSKONTROLL                                           
137301     .                                                                    
137401     EJECT                                                                
137501 IMS-GU-WDB501  SECTION.                                                  
137601     MOVE 'IMS-GU-WDB501     '      TO WS-SEKTION                         
137701                                                                          
137801     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X                            
137901                    '!WDB501KY =' W-WDB501KY-DEF ')'                      
138001           DELIMITED BY SIZE INTO SSA1                                    
138101     MOVE '  GE'   TO GODK-STATUSKODER                                    
138201     CALL CBLTDLI USING GU WDB5-PCB DLI-IO-WDB501 SSA1                    
138301     MOVE WDB5-STATUS-CODE TO STATUS-WS                                   
138401     PERFORM IMS-STATUSKONTROLL                                           
138501     .                                                                    
138601     SKIP3                                                                
138701 IMS-GU-WDE601 SECTION.                                                   
138801     MOVE 'IMS-GU-WDE601     '      TO WS-SEKTION                         
138901                                                                          
139001     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
139101          DELIMITED BY SIZE INTO SSA1                                     
139201     MOVE '  GE' TO GODK-STATUSKODER                                      
139301     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
139401     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
139501     PERFORM IMS-STATUSKONTROLL                                           
139601     .                                                                    
139701     SKIP3                                                                
139801 IMS-GU-WDE611     SECTION.                                               
139901     MOVE 'IMS-GU-WDE611     '      TO WS-SEKTION                         
140001                                                                          
140101     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
140201          DELIMITED BY SIZE INTO SSA1                                     
140301     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
140401          DELIMITED BY SIZE INTO SSA2                                     
140501     MOVE '  GE' TO GODK-STATUSKODER                                      
140601     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
140701     MOVE WDE6-STATUS-CODE TO STATUS-WS-E6                                
140801     PERFORM IMS-STATUSKONTROLL                                           
140901     .                                                                    
141001     EJECT                                                                
141101 IMS-GU-WDK601 SECTION.                                                   
141201     MOVE 'IMS-GU-WDK601'           TO WS-SEKTION                         
141301                                                                          
141401     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
141501          DELIMITED BY SIZE INTO SSA1                                     
141601     MOVE '    ' TO GODK-STATUSKODER                                      
141701     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
141801     MOVE WDK6-STATUS-CODE TO STATUS-WS-K6                                
141901     PERFORM IMS-STATUSKONTROLL                                           
142001     .                                                                    
142101     EJECT                                                                
142201 IMS-GU-WDE211     SECTION.                                               
142301     MOVE 'IMS-GHU-WDE211    '      TO WS-SEKTION                         
142401                                                                          
142501     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
142601          DELIMITED BY SIZE INTO SSA1                                     
142701     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
142801          DELIMITED BY SIZE INTO SSA2                                     
142901     MOVE '  GE' TO GODK-STATUSKODER                                      
143001     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2               
143101     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
143201     PERFORM IMS-STATUSKONTROLL                                           
143301     .                                                                    
143401     EJECT                                                                
143501 IMS-4735-GET-ROOT SECTION.                                               
143601                                                                          
143701     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4735-X ')'                    
143801            DELIMITED BY SIZE INTO SSA1                                   
143901     MOVE '  GE' TO GODK-STATUSKODER                                      
144001     CALL  CBLTDLI  USING GU   4735-PCB DLI-IO-WDGX4735 SSA1              
144101     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
144201     PERFORM IMS-STATUSKONTROLL                                           
144301     .                                                                    
144401     SKIP2                                                                
144501 IMS-4735-GET-SEGMENT SECTION.                                            
144601                                                                          
144701     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4735-X ')'                    
144801            DELIMITED BY SIZE INTO SSA1                                   
144901     MOVE 'WDGX4735' TO SSA2                                              
145001     MOVE '  GE' TO GODK-STATUSKODER                                      
145101     CALL CBLTDLI  USING GNP  4735-PCB DLI-IO-WDGX4735 SSA1 SSA2          
145201     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
145301     PERFORM IMS-STATUSKONTROLL                                           
145401     .                                                                    
145501     EJECT                                                                
145601 IMS-GU-WDB201     SECTION.                                               
145701     MOVE 'IMS-GU-WDB201'     TO WS-SEKTION                               
145801                                                                          
145901     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
146001          DELIMITED BY SIZE INTO SSA1                                     
146101     MOVE '    '              TO GODK-STATUSKODER                         
146201                                                                          
146301     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
146401     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
146501     PERFORM IMS-STATUSKONTROLL                                           
146601     .                                                                    
146701     SKIP2                                                                
146801 IMS-GU-WDK611     SECTION.                                               
146901     MOVE 'IMS-GU-WDK611'     TO WS-SEKTION                               
147001                                                                          
147101     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147201          DELIMITED BY SIZE INTO SSA1                                     
147301     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
147401          DELIMITED BY SIZE INTO SSA2                                     
147501                                                                          
147601     MOVE '  GE'              TO GODK-STATUSKODER                         
147701     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
147801     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
147901     PERFORM IMS-STATUSKONTROLL                                           
148001     .                                                                    
148101     SKIP2                                                                
148201 IMS-GET-1165-WDR2 SECTION.                                               
148301     MOVE 'GET-1165-WDR2   ' TO WS-SEKTION                                
148401                                                                          
148501     MOVE SPACE              TO SSA1                                      
148601                                SSA2                                      
148701                                                                          
148801     STRING 'WL116501(WDGXKEY  =' W-1165KEY-X ')'                         
148901          DELIMITED BY SIZE INTO SSA1                                     
149001     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
149101          DELIMITED BY SIZE INTO SSA2                                     
149201                                                                          
149301     MOVE '  GE'             TO GODK-STATUSKODER                          
149401     CALL CBLTDLI USING GU 1165-PCB DLI-IO-WL1168 SSA1 SSA2               
149501     MOVE 1165-STATUS-CODE   TO STATUS-WS                                 
149601     PERFORM IMS-STATUSKONTROLL                                           
149701     .                                                                    
149801     EJECT                                                                
149901 IMS-STATUSKONTROLL SECTION.                                              
150001     MOVE 'IMS-STATUSKONTR.'        TO WS-SEKTION                         
150101                                                                          
150201     SET STATUS-IX TO 1                                                   
150301     SEARCH GODK-STATUS                                                   
150401       AT END                                                             
150501         CALL FELLOG                                                      
150601       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150701         CONTINUE                                                         
150801     END-SEARCH                                                           
150901     .                                                                    
151001     EJECT                                                                
