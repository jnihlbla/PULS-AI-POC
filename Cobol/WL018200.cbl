000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     WL018200.                                                
000301 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000401 DATE-WRITTEN.   04/11/11.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    NAME:       'CARPARTS.LDC.BOOKINGINFO'                               
000801*                                                                         
000901*    FUNCTION:                                                            
001001*        ---------------------------------------------------------        
001101*        ADDITIONAL IN RELEASE 04:3                                       
001201*        SHOWS SUBTOTALS PER SENDING WITH FOLLOWING INPUT KEYS:           
001301*        IDSHIPM AND IDDC                                                 
001401*        ---------------------------------------------------------        
001501*        PROGRAM SHOWS BOTH PACKAGE INFORMATION BEFORE SHIPPING           
001601*        AND AFTER SHIPPING DEPENDING ON THE INPUT KEYS AS BELOW          
001701*        1. IDSHIPM  GIVEN - INFORMATION AFTER SHIPPING                   
001801*        2. IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                  
001901*        BOTH IDSHIPM AND IDTRPTNR TOGETHER ARE NOT ALLOWED               
002001*        INFORMATION SHOWN ON SCREEN -                                    
002101*        SUMMERY FIELDS: TOT NO CASE,TOT WEIGHT,TOT VOLUME,TOT            
002201*        VALUE,TOT NETWEIGHT                                              
002301*                                                                         
002401*        WL018200 PROGRAM IS A REPLICA OF W4062400 PROGRAM                
002501*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002601*                                                                         
002701*---- OBS!                                                                
002801*        PROGRAMET ÄR INTE ANPASSAT MED IDPARTNR-EXP FÖR ATT TA           
002901*        FRAM RÄTT KDVALISO EFTERSOM MAN INTE VISAR NÅGRA VÄRDEN          
003001*        PÅ BILDEN. MAN MÅSTE GÖRA DET IFALL FÖRUTSÄTT. ÄNDRAS!           
003101*---- OBS!                                                                
003201*                                                                         
003300*    INDATA.                                                              
003400*        TRANSACTION: WL0182T                                             
003500*        REQUEST:     WL0182I1                                            
003600*                                                                         
003700*    OUTDATA.                                                             
003800*        RESPONSE:    WL0182O1                                            
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800     SKIP3                                                                
004900 FILE SECTION.                                                            
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200 77  IDPGM                       PIC X(08)   VALUE 'WL018200'.            
005300                                                                          
005400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005600 77  KDRC-DISPLAY                PIC Z(5).                                
005700                                                                          
005800 77  FELTEXT                     PIC X(16)   VALUE SPACE.                 
005900                                                                          
006000 77  YES                         PIC X       VALUE 'J'.                   
006100 77  NOO                         PIC X       VALUE 'N'.                   
006201 77  WS-DCS-IDFTG                PIC XX      VALUE 'N'.                   
006301 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006401 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
006501                                                                          
006601*    --- INDEX FOR SCROLL LINES                                           
006701 77  INDX                        PIC S9(4)  VALUE +0.                     
006801 77  WS-COUNT                    PIC S9(4)  VALUE +0.                     
006901 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
007001*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
007101                                                                          
007201                                                                          
007301 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007401     88  KEYS-OK                             VALUE 'J'.                   
007501     88  KEYS-WRONG                          VALUE 'N'.                   
007601                                                                          
007701 77  W-IDSHIPM-GIVEN             PIC X       VALUE 'N'.                   
007801     88  IDSHIPM-GIVEN                       VALUE 'J'.                   
007901     88  IDSHIPM-NOT-GIVEN                   VALUE 'N'.                   
008001                                                                          
008101 77  W-IDTRPTNR-GIVEN            PIC X        VALUE 'N'.                  
008201     88  IDTRPTNR-GIVEN                       VALUE 'J'.                  
008301     88  IDTRPTNR-NOT-GIVEN                   VALUE 'N'.                  
008401                                                                          
008501     EJECT                                                                
008601*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008701 01  GENERAL-SUBPROGRAMS.                                                 
008801     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008901     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009001     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009101     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009201     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
009301     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
009401     SKIP3                                                                
009501*    --- PARAMETRAR TO W510CURR                                           
009601*01  -COPY W510CURR                                                       
009701     EJECT                                                                
009801*    --- PARAMETERS TO ABEND                                              
009901                                                                          
010001 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010101 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010201 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010301     EJECT                                                                
010401*                                                                         
010501 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010601     SKIP3                                                                
010701*01  -COPY WZ01SUB                                                        
010801     EJECT                                                                
010901 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011001     SKIP3                                                                
011101 01  REQU-AREA.                                                           
011201*    03  -COPY WZ01REQU                                                   
011301*    03  -COPY WL0182I1                                                   
011401     EJECT                                                                
011501 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011601     SKIP3                                                                
011701 01  RESP-AREA.                                                           
011801*    03  -COPY WZ01RESP                                                   
011901*    03  -COPY WL0182O1                                                   
012001     EJECT                                                                
012101                                                                          
012201 01  MESSAGE-CODES.                                                       
012301*    03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
012401*    03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012501*    03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012601*    03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012701     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012801*    03  SHIPMENT-MISSING        PIC X(3)    VALUE '341'.                 
012901     03  SYS-ERR                 PIC X(3)    VALUE '099'.                 
013001     EJECT                                                                
013101*01  W-NOT-ALLOWED-TEXT          PIC X(55)   VALUE                        
013201*       'SHIPM NO AND TRANSPORT ARE NOT ALLOWED AT THE SAME TIME'.        
013301                                                                          
013401*01  FILLER-1.                                                            
013501*    03  FILLER                  PIC X(40)                                
013601*        VALUE '351 ONLY BILL-IT MARKET ALLOWED        '.                 
013701*01  FILLER REDEFINES FILLER-1.                                           
013801*    03  ERR-NOT-BILL-IT-MARKET      OCCURS 1  PIC X(40).                 
013901     EJECT                                                                
014001 01  FILLER                      PIC  X(16)  VALUE                        
014101                                                'W411EXCH AREA '.         
014201*01 -COPY W411EXCH                                                        
014301                                                                          
014401*      ----DISTR-DEALER-PRICE------                                       
014501*01 -COPY WWDIST79                                                        
014601                                                                          
014701     EJECT                                                                
014801*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
014901*                                                                         
015001 01  FILLER                      PIC X(16)   VALUE 'SAVE AREA'.           
015101     SKIP3                                                                
015201 01  SAVE-AREA.                                                           
015301** FOR 1 WHEN IDSHIPM GIVEN  - INFORMATION AFTER SHIPPING                 
015401     03  SAVE-SCROLL-1.                                                   
015501         05  SAVE-IDPRODNR-ENTER PIC S9(7)  VALUE ZERO COMP-3.            
015601         05  SAVE-IDPRODNR-NEXT  PIC S9(7)  VALUE ZERO COMP-3.            
015701         05  SAVE-IDKUNDNR-ENTER PIC S9(7)  VALUE ZERO COMP-3.            
015801         05  SAVE-IDKUNDNR-NEXT  PIC S9(7)  VALUE ZERO COMP-3.            
015901         05  SAVE-IDKOLLI-ENTER  PIC S9(5)  VALUE ZERO COMP-3.            
016001         05  SAVE-IDKOLLI-NEXT   PIC S9(5)  VALUE ZERO COMP-3.            
016101         05  SAVE-IDDISTR-ENTER  PIC S9(5)  VALUE ZERO COMP-3.            
016201         05  SAVE-IDDISTR-NEXT   PIC S9(5)  VALUE ZERO COMP-3.            
016301** FOR 2 WHEN IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                
016401     03  SAVE-WDE6CSEQ-ENTER.                                             
016501         05  SAVE-IDTRPTNR-ENTER       PIC S9(3)     COMP-3.              
016601         05  SAVE-DARFS-ENTER          PIC 9(12).                         
016701         05  SAVE-ADKOLLI-ENTER.                                          
016801             07 SAVE-ADCLGEO-ENTER.                                       
016901                09 SAVE-IDDC-ENTER     PIC X(2).                          
017001                09 SAVE-ADFLGEO-ENTER  PIC X(3).                          
017101             07 SAVE-ADFLOMR-ENTER     PIC S9(3)     COMP-3.              
017201             07 SAVE-ADRUTNIV-ENTER    PIC S9(3)     COMP-3.              
017301             07 SAVE-ADVMODUL-ENTER    PIC S9(3)     COMP-3.              
017401         05  SAVE-TRP-IDPRODNR-ENTER   PIC S9(7)     COMP-3.              
017501         05  SAVE-TRP-IDKOLLI-ENTER    PIC S9(5)     COMP-3.              
017601     03  SAVE-WDE6CSEQ-NEXT.                                              
017701         05  SAVE-IDTRPTNR-NEXT        PIC S9(3)     COMP-3.              
017801         05  SAVE-DARFS-NEXT           PIC 9(12).                         
017901         05  SAVE-ADKOLLI-NEXT.                                           
018001             07 SAVE-ADCLGEO-NEXT.                                        
018101                09 SAVE-IDDC-NEXT      PIC X(2).                          
018201                09 SAVE-ADFLGEO-NEXT   PIC X(3).                          
018301             07 SAVE-ADFLOMR-NEXT      PIC S9(3)     COMP-3.              
018401             07 SAVE-ADRUTNIV-NEXT     PIC S9(3)     COMP-3.              
018501             07 SAVE-ADVMODUL-NEXT     PIC S9(3)     COMP-3.              
018601         05  SAVE-TRP-IDPRODNR-NEXT    PIC S9(7)     COMP-3.              
018701         05  SAVE-TRP-IDKOLLI-NEXT     PIC S9(5)     COMP-3.              
018801     03  SAVE-TOTAL.                                                      
018901         05  SAVE-KVKOLLI-SKP    PIC 9(5).                                
019001         05  SAVE-VKORDBTO-SKP   PIC S9(6)V9(1).                          
019101         05  SAVE-VLORDBTO-SKP   PIC S9(4)V9(3).                          
019201         05  SAVE-SUORDV-SKEPPN  PIC S9(9)V9(2).                          
019301         05  SAVE-SUORDV-SKEPPN-EXP  PIC S9(9)V9(2).                      
019401         05  SAVE-VKORDNTO-SKP   PIC S9(6)V9(1).                          
019501         05  SAVE-SUORDV-LOCPREL-SKP                                      
019601                                 PIC S9(9)V9(2).                          
019701     03  SAVE-PRKURS             PIC S9(6)V9(5)    COMP-3.                
019801     EJECT                                                                
019901 01  FILLER                      PIC X(16)   VALUE 'TOTAL AREA'.          
020001     SKIP3                                                                
020101 01  W-TOTAL.                                                             
020201     03  W-SUORDV-TOT-SEK        PIC S9(9)V9(2)    VALUE ZERO.            
020301     03  W-SUORDV-TOT-EXP        PIC S9(9)V9(2)    VALUE ZERO.            
020401     03  W-SUORDV-LOC            PIC S9(9)V9(2)    VALUE ZERO.            
020501     03  W-SUORDV-LOCPREL        PIC S9(9)V9(2)    VALUE ZERO.            
020601     03  W-SUORDV-LOC-SEK        PIC S9(9)V9(2)    VALUE ZERO.            
020701     03  W-SUORDV-LOCPREL-SEK    PIC S9(9)V9(2)    VALUE ZERO.            
020801 01  FILLER                      PIC X(16)   VALUE 'W-SKP AREA'.          
020901     SKIP3                                                                
021001 01  W-SKP.                                                               
021101     03  W-KVKOLLI-SKP           PIC 9(5)          VALUE ZERO.            
021201     03  W-VKORDBTO-SKP          PIC S9(6)V9(1)    VALUE ZERO.            
021301     03  W-VLORDBTO-SKP          PIC S9(4)V9(3)    VALUE ZERO.            
021401     03  W-SUORDV-SKEPPN         PIC S9(9)V9(2)    VALUE ZERO.            
021501     03  W-SUORDV-SKEPPN-EXP     PIC S9(9)V9(2)    VALUE ZERO.            
021601     03  W-VKORDNTO-SKP          PIC S9(6)V9(1)    VALUE ZERO.            
021701     03  W-SUORDV-LOCPREL-SKP    PIC S9(9)V9(2)    VALUE ZERO.            
021801*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
021901*                                                                         
022001     EJECT                                                                
022101*    --- WORK-AREAS FOR IMS-SECTIONS                                      
022201*                                                                         
022301 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022401     SKIP3                                                                
022501 01  KEYS-TO-DLI.                                                         
022601*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
022701     03  W-IDSHIPM-X.                                                     
022801         05  W-WDE101-GHU-IDSHIPM   PIC 9(7)    VALUE ZERO.               
022901*                                                                         
023001     03  W-WDE101-IDSHIPM-X.                                              
023101         05  W-WDE101-IDSHIPM       PIC 9(7)    VALUE ZERO.               
023201*                                                                         
023301     03  W-WDE111KY-X.                                                    
023401         05  W-WDE111-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.        
023501         05  W-WDE111-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.        
023601*                                                                         
023701     03  W-IDDC-X.                                                        
023801         05  W-IDDC                 PIC X(2)    VALUE SPACE.              
023901*                                                                         
024001     03  W-IDDISTR-X.                                                     
024101         05  W-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.        
024201*                                                                         
024301     03  W-IDKUNDNR-X.                                                    
024401         05  W-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.        
024501*                                                                         
024601     03  W-WDE111KY-MIN.                                                  
024701         05  W-WDE111-IDDISTR-MIN   PIC S9(5)   VALUE ZERO COMP-3.        
024801         05  W-WDE111-IDKUNDNR-MIN  PIC S9(7)   VALUE ZERO COMP-3.        
024901*                                                                         
025001     03  W-WDE111KY-MAX.                                                  
025101         05  W-WDE111-IDDISTR-MAX   PIC S9(5) VALUE 99999 COMP-3.         
025201         05  W-WDE111-IDKUNDNR-MAX PIC S9(7) VALUE 9999999 COMP-3.        
025301*                                                                         
025401     03  W-WDE121KY-MIN.                                                  
025501         05  W-WDE121-IDPRODNR-MIN  PIC S9(7)           COMP-3.           
025601         05  W-WDE121-IDKOLLI-MIN   PIC S9(5)           COMP-3.           
025701*                                                                         
025801     03  W-WDE121KY-MAX.                                                  
025901         05  W-WDE121-IDPRODNR-MAX  PIC S9(7)           COMP-3.           
026001         05  W-WDE121-IDKOLLI-MAX   PIC S9(5)           COMP-3.           
026101*                                                                         
026201     03  W-WDE601-IDPRODNR-X.                                             
026301         05  W-WDE601-IDPRODNR      PIC S9(7)   VALUE ZERO COMP-3.        
026401*                                                                         
026501     03  W-WDE611-IDKOLLI-X.                                              
026601         05  W-WDE611-IDKOLLI       PIC S9(5)   VALUE ZERO COMP-3.        
026701*                                                                         
026801     03  W-WDE6CSEQ-MIN.                                                  
026901         05  W-WDE611-IDTRPTNR-MIN         PIC S9(3)    COMP-3.           
027001         05  W-WDE611-DARFS-MIN            PIC 9(12).                     
027101         05  W-WDE611-ADKOLLI-MIN.                                        
027201             07  W-WDE611-ADCLGEO-MIN.                                    
027301                 09  W-WDE611-IDDC-MIN     PIC X(2).                      
027401                 09  W-WDE611-ADFLGEO-MIN  PIC X(3).                      
027501             07  W-WDE611-ADFLOMR-MIN      PIC S9(3)    COMP-3.           
027601             07  W-WDE611-ADRUTNIV-MIN     PIC S9(3)    COMP-3.           
027701             07  W-WDE611-ADVMODUL-MIN     PIC S9(3)    COMP-3.           
027801*                                                                         
027901     03  W-WDE6CSEQ-MAX.                                                  
028001         05  W-WDE611-IDTRPTNR-MAX         PIC S9(3)    COMP-3.           
028101         05  W-WDE611-DARFS-MAX            PIC 9(12).                     
028201         05  W-WDE611-ADKOLLI-MAX.                                        
028301             07  W-WDE611-ADCLGEO-MAX.                                    
028401                 09  W-WDE611-IDDC-MAX     PIC X(2).                      
028501                 09  W-WDE611-ADFLGEO-MAX  PIC X(3).                      
028601             07  W-WDE611-ADFLOMR-MAX      PIC S9(3)    COMP-3.           
028701             07  W-WDE611-ADRUTNIV-MAX     PIC S9(3)    COMP-3.           
028801             07  W-WDE611-ADVMODUL-MAX     PIC S9(3)    COMP-3.           
028901*                                                                         
029001     03  W-WDE4F1KY-MIN-X.                                                
029101         05  W-E4-IDPRODNR-MIN   PIC S9(07)   VALUE ZERO COMP-3.          
029201         05  W-E4-IDKOLLI-MIN    PIC S9(05)   VALUE ZERO COMP-3.          
029301         05  FILLER              PIC X(22)    VALUE LOW-VALUE.            
029401*                                                                         
029501     03  W-WDE4F1KY-MAX-X.                                                
029601         05  W-E4-IDPRODNR-MAX   PIC S9(07)   VALUE ZERO COMP-3.          
029701         05  W-E4-IDKOLLI-MAX    PIC S9(05)   VALUE ZERO COMP-3.          
029801         05  FILLER              PIC X(22)    VALUE HIGH-VALUE.           
029901*                                                                         
030001     03  W-WDB201-IDGMT-X.                                                
030101         05 W-WDB201-IDDISTR     PIC S9(5) VALUE ZERO COMP-3.             
030201         05 W-WDB201-IDKUNDNR    PIC S9(7) VALUE ZERO COMP-3.             
030301*                                                                         
030401     03  W-WDB201-IDGMT-MIN-X.                                            
030501         05 W-WDB201-IDDISTR-MIN   PIC S9(5) VALUE ZERO COMP-3.           
030601         05 W-WDB201-IDKUNDNR-MIN  PIC S9(7) VALUE ZERO COMP-3.           
030701*                                                                         
030801     03  W-WDB201-IDGMT-MAX-X.                                            
030901         05 W-WDB201-IDDISTR-MAX   PIC S9(5) VALUE ZERO COMP-3.           
031001         05 W-WDB201-IDKUNDNR-MAX  PIC S9(7) VALUE ZERO COMP-3.           
031101*                                                                         
031201     03  W-WDB101KY-X.                                                    
031301         05 W-WDB101-IDPARTNR      PIC X(9)  VALUE SPACE.                 
031401         05 W-WDB101-IDFTG         PIC 9(2)  VALUE ZERO.                  
031501*                                                                         
032001     03  W-IDDC-B6-X.                                                     
032101         05 W-IDDC-B6             PIC X(2).                               
032201*                                                                         
032300     SKIP2                                                                
032400*    --- STATUS-KOD FRÅN IMS                                              
032500 01  STATUS-WS                   PIC XX.                                  
032600     88  SEGMENT-FOUND                       VALUE '  '.                  
032700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
032800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
032900     SKIP2                                                                
033000 01  GOOD-STATUSCODES.                                                    
033100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033200     SKIP3                                                                
033300 01  SSA1                        PIC X(144).                              
033400 01  SSA2                        PIC X(128).                              
033500     EJECT                                                                
033600*    --- IMS FUNCTION CODES                                               
033700*01  -COPY W0003                                                          
033800     EJECT                                                                
033900*    ---  DLI INPUT-OUTPUT AREA                                           
034000                                                                          
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
034200 01  DLI-IO-WDE101.                                                       
034300*    03  -COPY WDE101                                                     
034400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
034500 01  DLI-IO-WDE111.                                                       
034600*    03  -COPY WDE111                                                     
034700     EJECT                                                                
034800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
034900 01  DLI-IO-WDE121.                                                       
035000*    03  -COPY WDE121                                                     
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611-01'.                   
035200 01  DLI-IO-WDE611-01.                                                    
035300   03  DLI-IO-WDE611.                                                     
035400*    05  -COPY WDE611                                                     
035500   03  DLI-IO-WDE601.                                                     
035600*    05  -COPY WDE601                                                     
035700     EJECT                                                                
035800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4F1'.                      
035900 01  DLI-IO-WDE4F1.                                                       
036000*    03  -COPY WDE4F1                                                     
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
036200 01  DLI-IO-WDB201.                                                       
036300*    03  -COPY WDB201                                                     
036400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
036500 01  DLI-IO-WDB101.                                                       
036600*    03  -COPY WDB101                                                     
037001 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
037101 01   DLI-IO-AREA-B601.                                                   
037201*     03  -COPY WDB601                                                    
037301                                                                          
037400     EJECT                                                                
037500 LINKAGE SECTION.                                                         
037600 01  MSG-PCB                     PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008  -PRE WDE1-                                              
037900     05  FILLER                  PIC X.                                   
038000                                                                          
038100*01  -COPY W0008  -PRE WDE6-                                              
038200     05  FILLER                  PIC X.                                   
038300                                                                          
038400*01  -COPY W0008  -PRE WDE6CSQ-                                           
038500     05  FILLER                  PIC X.                                   
038600                                                                          
038700*01  -COPY W0008  -PRE WDE4F-                                             
038800     05  FILLER                  PIC X.                                   
038900                                                                          
039000*01  -COPY W0008  -PRE WDB2-                                              
039100     05  FILLER                  PIC X.                                   
039200                                                                          
039300*01  -COPY W0008  -PRE WDB1-                                              
039400     05  FILLER                  PIC X.                                   
039500                                                                          
039600*01  -COPY W0008  -PRE WDG2-                                              
039700     05  FILLER                  PIC X.                                   
039801                                                                          
039901*01  -COPY W0008  -PRE WDB6-                                              
040001     05  FILLER                  PIC X.                                   
040101                                                                          
040200     EJECT                                                                
040300                                                                          
040400 PROCEDURE DIVISION  USING MSG-PCB WDE1-PCB WDE6-PCB                      
040500     WDE6CSQ-PCB WDE4F-PCB WDB2-PCB WDB1-PCB WDG2-PCB                     
040601     WDB6-PCB.                                                            
040700                                                                          
040800 MAIN SECTION.                                                            
040900     ENTRY 'DLITCBL' USING MSG-PCB WDE1-PCB WDE6-PCB                      
041000     WDE6CSQ-PCB WDE4F-PCB WDB2-PCB WDB1-PCB WDG2-PCB                     
041101     WDB6-PCB.                                                            
041200                                                                          
041300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
041400     IF SUB-KDRC = 0                                                      
041500       IF REQU-KDPGMACT          = 'S'                                    
041600         PERFORM A-INIT                                                   
041700         PERFORM B-CHECK-KEYS                                             
041800         IF KEYS-OK                                                       
041900             PERFORM F-READ-SHOW-INFO                                     
042000         END-IF                                                           
042100       ELSE                                                               
042200         MOVE SYS-ERR      TO RESP-IDMSG-ERROR                            
042300       END-IF                                                             
042400                                                                          
042500       PERFORM S02-RETURN-RESPONSE                                        
042600                                                                          
042700     END-IF                                                               
042800                                                                          
042900     MOVE ZERO TO RETURN-CODE                                             
043000     GOBACK                                                               
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 A-INIT SECTION.                                                          
043500                                                                          
043600                                                                          
043700                                                                          
043800                                                                          
043900                                                                          
044000     MOVE  ZERO             TO  W-WDE121-IDPRODNR-MIN                     
044100                                W-WDE121-IDKOLLI-MIN                      
044200                                W-WDE611-IDTRPTNR-MIN                     
044300                                W-WDE611-DARFS-MIN                        
044400                                W-WDE611-ADFLOMR-MIN                      
044500                                W-WDE611-ADRUTNIV-MIN                     
044600                                W-WDE611-ADVMODUL-MIN                     
044700                                W-WDB201-IDDISTR-MIN                      
044800                                W-WDB201-IDKUNDNR-MIN                     
044900                                                                          
045000     MOVE LOW-VALUE         TO  W-WDE611-IDDC-MIN                         
045100                                W-WDE611-ADFLGEO-MIN                      
045200                                                                          
045300     MOVE  ALL '9'          TO  W-WDE121-IDPRODNR-MAX                     
045400                                W-WDE121-IDKOLLI-MAX                      
045500                                W-WDE611-IDTRPTNR-MAX                     
045600                                W-WDE611-DARFS-MAX                        
045700                                W-WDE611-ADFLOMR-MAX                      
045800                                W-WDE611-ADRUTNIV-MAX                     
045900                                W-WDE611-ADVMODUL-MAX                     
046000                                W-WDB201-IDDISTR-MAX                      
046100                                W-WDB201-IDKUNDNR-MAX                     
046200                                                                          
046300     MOVE HIGH-VALUE        TO  W-WDE611-IDDC-MAX                         
046400                                W-WDE611-ADFLGEO-MAX                      
046500     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
046510     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
046600                                                                          
046700                                                                          
046800     MOVE ALL '+'           TO RESP-AREA                                  
046900     MOVE SPACE             TO RESP-IDMSG-ERROR                           
047000                               RESP-IDMSG-INFO                            
047100                               RESP-IDELMT-ERROR                          
047200     MOVE 001               TO RESP-IDMSGVER                              
047300     MOVE ZERO              TO RESP-KVRADER                               
047400     MOVE ZERO              TO WS-COUNT                                   
047500                                                                          
047600     .                                                                    
047700     EJECT                                                                
047800 B-CHECK-KEYS SECTION.                                                    
047900                                                                          
048000        IF REQU-IDSHIPM-KEY NOT = ALL '+'                                 
048100           IF REQU-IDSHIPM-KEY NUMERIC                                    
048200              IF REQU-IDSHIPM-KEY > ZERO                                  
048300                MOVE REQU-IDSHIPM-KEY TO W-IDSHIPM-X                      
048400                PERFORM S03-NOLLA-SAVE-SHIPM                              
048500              END-IF                                                      
048600           END-IF                                                         
048700        END-IF                                                            
048800                                                                          
048900     MOVE YES TO KEYS-SW                                                  
049000     MOVE SPACE TO RESP-IDELMT-ERROR                                      
049100                                                                          
049200                                                                          
049300*    -- CHECK OF KEYS                                                     
049400     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
049500       IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY >= ZERO           
049600          MOVE REQU-IDDISTR-KEY TO W-WDE111-IDDISTR                       
049700                                   W-WDE111-IDDISTR-MIN                   
049800                                   W-IDDISTR                              
049900       ELSE                                                               
050000         MOVE NOO       TO KEYS-SW                                        
050101         MOVE ZERO      TO REQU-IDDISTR-KEY                               
050200       END-IF                                                             
050300     ELSE                                                                 
050400       IF REQU-IDDISTR-KEY NUMERIC                                        
050500          MOVE REQU-IDDISTR-KEY    TO RESP-IDDISTR-KEY                    
050600          IF REQU-IDDISTR-KEY >= ZERO                                     
050700             MOVE REQU-IDDISTR-KEY TO W-WDE111-IDDISTR                    
050800                                      W-WDE111-IDDISTR-MIN                
050900                                      W-IDDISTR                           
051000          ELSE                                                            
051100             MOVE NOO         TO KEYS-SW                                  
051201             MOVE ZERO        TO REQU-IDDISTR-KEY                         
051300          END-IF                                                          
051400       ELSE                                                               
051500          MOVE NOO            TO KEYS-SW                                  
051601         MOVE ZERO            TO REQU-IDDISTR-KEY                         
051700       END-IF                                                             
051800                                                                          
051900     END-IF                                                               
052000                                                                          
052100     MOVE REQU-IDDISTR-KEY TO DIST79-IDDISTR                              
052200     IF REQU-IDDISTR-KEY = ZERO                                           
052300        MOVE YES              TO KEYS-SW                                  
052400     END-IF                                                               
052500                                                                          
052600     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
052700       IF REQU-IDKUNDNR-KEY NUMERIC AND REQU-IDKUNDNR-KEY >= ZERO         
052800         MOVE REQU-IDKUNDNR-KEY TO W-WDE111-IDKUNDNR                      
052900                                  W-WDE111-IDKUNDNR-MIN                   
053000                                  W-IDKUNDNR                              
053100                                  RESP-IDKUNDNR-KEY                       
053200       ELSE                                                               
053300         MOVE NOO       TO KEYS-SW                                        
053400       END-IF                                                             
053500     ELSE                                                                 
053600       IF REQU-IDKUNDNR-KEY NUMERIC                                       
053700         IF REQU-IDKUNDNR-KEY >= ZERO                                     
053800          MOVE REQU-IDKUNDNR-KEY TO W-WDE111-IDKUNDNR                     
053900                                   W-WDE111-IDKUNDNR-MIN                  
054000                                   W-IDKUNDNR                             
054100         ELSE                                                             
054200            MOVE NOO              TO KEYS-SW                              
054300         END-IF                                                           
054400       ELSE                                                               
054500          MOVE NOO              TO KEYS-SW                                
054600       END-IF                                                             
054700     END-IF                                                               
054800                                                                          
054900     IF REQU-IDTRPTNR-KEY NOT = ALL '+'                                   
055000       IF REQU-IDTRPTNR-KEY NOT NUMERIC                                   
055100         MOVE NOO       TO KEYS-SW                                        
055200       ELSE                                                               
055300         MOVE REQU-IDTRPTNR-KEY  TO RESP-IDTRPTNR-KEY                     
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     IF REQU-IDSHIPM-KEY NOT = ALL '+'                                    
055800       IF REQU-IDSHIPM-KEY  NOT NUMERIC                                   
055900         MOVE NOO       TO KEYS-SW                                        
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     IF REQU-IDTRPTNR-KEY NUMERIC AND REQU-IDTRPTNR-KEY > ZERO            
056400        MOVE REQU-IDTRPTNR-KEY TO W-WDE611-IDTRPTNR-MIN                   
056500                                 W-WDE611-IDTRPTNR-MAX                    
056600        MOVE YES              TO W-IDTRPTNR-GIVEN                         
056700     END-IF                                                               
056800                                                                          
056900     IF REQU-IDSHIPM-KEY NUMERIC AND REQU-IDSHIPM-KEY > ZERO              
057000        MOVE REQU-IDSHIPM-KEY TO W-WDE101-IDSHIPM                         
057100                                 W-WDE101-GHU-IDSHIPM                     
057200        MOVE YES              TO W-IDSHIPM-GIVEN                          
057300        MOVE REQU-IDSHIPM-KEY TO RESP-IDSHIPM-KEY                         
057400                                                                          
057500     END-IF                                                               
057600*                                                                         
057700     IF IDSHIPM-GIVEN AND IDTRPTNR-GIVEN                                  
057800        MOVE NOO              TO KEYS-SW                                  
057900        MOVE '278'            TO RESP-IDMSG-INFO                          
058000     ELSE                                                                 
058100        IF IDSHIPM-NOT-GIVEN AND IDTRPTNR-NOT-GIVEN                       
058200           MOVE NOO              TO KEYS-SW                               
058300        END-IF                                                            
058400     END-IF                                                               
058500                                                                          
058600     MOVE REQU-IDDC-KEY          TO RESP-IDDC-KEY                         
058700                                    W-WDE611-IDDC-MIN                     
058800                                    W-WDE611-IDDC-MAX                     
058900                                    W-IDDC                                
059001                                                                          
059101     MOVE REQU-IDDC-KEY          TO W-IDDC-B6                             
059201     PERFORM IMS-GU-WDB601                                                
059301     MOVE DCS-IDFTG              TO WS-DCS-IDFTG                          
059400                                                                          
059500     IF KEYS-WRONG                                                        
059600       MOVE '043'         TO RESP-IDMSG-ERROR                             
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 F-READ-SHOW-INFO SECTION.                                                
060100                                                                          
060200     IF IDSHIPM-GIVEN                                                     
060300       IF REQU-IDDISTR-KEY = ZERO AND REQU-IDKUNDNR-KEY = ZERO            
060400         PERFORM FC-READ-SUM-PER-SHIPMENT                                 
060500       ELSE                                                               
060600         PERFORM FA-READ-AFTER-SHIPMENT                                   
060700       END-IF                                                             
060800     END-IF                                                               
060900     IF IDTRPTNR-GIVEN                                                    
061000        PERFORM FB-READ-BEFORE-SHIPMENT                                   
061100     END-IF                                                               
061200                                                                          
061300                                                                          
061400     .                                                                    
061500     EJECT                                                                
061600                                                                          
061700 FA-READ-AFTER-SHIPMENT SECTION.                                          
061800                                                                          
061900     PERFORM IMS-GU-WDE111                                                
062000     IF SEGMENT-FOUND                                                     
062100        PERFORM IMS-GNP-WDE121                                            
062200     END-IF                                                               
062300                                                                          
062400     PERFORM S03-NOLLA-SAVE-TRP                                           
062500     IF SEGMENT-MISSING                                                   
062600        MOVE '025'                  TO RESP-IDMSG-ERROR                   
062700        MOVE 'IDTRP'                TO RESP-IDELMT-ERROR                  
062800        MOVE W-WDE121-IDPRODNR-MIN  TO SAVE-IDPRODNR-ENTER                
062900        MOVE W-WDE121-IDKOLLI-MIN   TO SAVE-IDKOLLI-ENTER                 
063000     ELSE                                                                 
063100*      -- POSITION FOR READING DATA TO UPPERMOST LINE                     
063200*      -- (NOT NECESSARY IF -MIN KEYS ARE DIRECTLY USED IN SSA)           
063300                                                                          
063400       MOVE +1                      TO INDX                               
063500       IF SEGMENT-FOUND                                                   
063600         MOVE SKOLLI-IDPRODNR       TO SAVE-IDPRODNR-ENTER                
063700         MOVE SKOLLI-IDKOLLI        TO SAVE-IDKOLLI-ENTER                 
063800       ELSE                                                               
063900         MOVE W-WDE121-IDPRODNR-MIN TO SAVE-IDPRODNR-ENTER                
064000         MOVE W-WDE121-IDKOLLI-MIN  TO SAVE-IDKOLLI-ENTER                 
064100       END-IF                                                             
064200                                                                          
064300       PERFORM UNTIL INDX > MAX-INDX                                      
064400         IF SEGMENT-FOUND                                                 
064500           MOVE SGMT-IDDISTR        TO DIST79-IDDISTR                     
064600           IF  DIST79-DEALER-PRICE                                        
064700                 IF  INDX = 1                                             
064800                    PERFORM S01-FIND-PRKURS                               
064900                 END-IF                                                   
065000              MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                  
065100              MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL              
065200              PERFORM S02-CONVERT-TO-SEK                                  
065300           ELSE                                                           
065400              MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                
065500              MOVE SKOLLI-SUORDV-LOCPREL                                  
065600                                       TO W-SUORDV-LOCPREL-SEK            
065700           END-IF                                                         
065800           MOVE SGMT-IDKUNDNR     TO RESP-IDKUNDNR (INDX)                 
065900           MOVE SKOLLI-IDORDNR7   TO RESP-IDORDNR5 (INDX)                 
066000           MOVE SKOLLI-IDKOLLI    TO RESP-IDKOLLI (INDX)                  
066100                                                                          
066200           MOVE SKOLLI-IDPRODNR   TO W-WDE601-IDPRODNR                    
066300           MOVE SKOLLI-IDKOLLI    TO W-WDE611-IDKOLLI                     
066400           PERFORM IMS-GU-WDE611                                          
066500           IF SEGMENT-FOUND                                               
066600              MOVE KOLLI-IDTULL   TO RESP-IDTULL (INDX)                   
066700              IF  KOLLI-IDPSN (1)  > ZERO                                 
066800                  MOVE   'Y'      TO RESP-FLFARLIG (INDX)                 
066900            END-IF                                                        
067000           ELSE                                                           
067100              MOVE SPACE          TO RESP-IDTULL (INDX)                   
067200                                     RESP-FLFARLIG (INDX)                 
067300           END-IF                                                         
067400                                                                          
067500           MOVE SKOLLI-DIKOLLIL   TO RESP-DIKOLLIL (INDX)                 
067600           MOVE SKOLLI-DIKOLLIB   TO RESP-DIKOLLIB (INDX)                 
067700           MOVE SKOLLI-DIKOLLIH   TO RESP-DIKOLLIH (INDX)                 
067800           MOVE SKOLLI-VKORDBTO-KOLLI                                     
067900                                  TO RESP-VKORDBTO-KOLLI (INDX)           
068000           MOVE SKOLLI-VLORDBTO-KOLLI                                     
068100                                  TO RESP-VLORDBTO-KOLLI (INDX)           
068200           MOVE SKOLLI-VKORDNTO-KOLLI                                     
068300                                  TO RESP-VKORDNTO-KOLLI (INDX)           
068400           COMPUTE W-SUORDV-TOT-SEK = SKOLLI-SUORDV    +                  
068500                                      W-SUORDV-LOC-SEK +                  
068600                                      W-SUORDV-LOCPREL-SEK                
068701           MOVE SKOLLI-SUORDV-EXP  TO W-SUORDV-TOT-EXP                    
068800           IF W-SUORDV-TOT-EXP > ZERO                                     
068901             MOVE W-SUORDV-TOT-EXP TO RESP-SUORDV-KOLLI (INDX)            
069001           ELSE                                                           
069101             MOVE W-SUORDV-TOT-SEK TO RESP-SUORDV-KOLLI (INDX)            
069201           END-IF                                                         
069300                                                                          
069400           IF W-SUORDV-LOCPREL-SEK   > ZERO                               
069500             MOVE '*'             TO RESP-TEASTRIX-KLI (INDX)             
069600           ELSE                                                           
069700             MOVE ' '             TO RESP-TEASTRIX-KLI (INDX)             
069800           END-IF                                                         
069900                                                                          
070000           ADD 1 TO WS-COUNT                                              
070100                                                                          
070200           MOVE WS-COUNT TO RESP-KVRADER                                  
070300               COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1              
070400               COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                 
070500                                         SKOLLI-VKORDBTO-KOLLI            
070600               COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                 
070700                                         SKOLLI-VLORDBTO-KOLLI            
070800               COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                 
070900                                         SKOLLI-VKORDNTO-KOLLI            
071000               COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                
071100                                         W-SUORDV-TOT-SEK                 
071201               COMPUTE W-SUORDV-SKEPPN-EXP = W-SUORDV-SKEPPN-EXP +        
071301                                         W-SUORDV-TOT-EXP                 
071400               COMPUTE W-SUORDV-LOCPREL-SKP                               
071500                                       = W-SUORDV-LOCPREL-SKP +           
071600                                         W-SUORDV-LOCPREL-SEK             
071700           PERFORM IMS-GNP-WDE121                                         
071800         END-IF                                                           
071900         ADD 1 TO INDX                                                    
072000                                                                          
072100       END-PERFORM                                                        
072200                                                                          
072300       IF SEGMENT-FOUND                                                   
072400         MOVE SKOLLI-IDPRODNR       TO SAVE-IDPRODNR-NEXT                 
072500         MOVE SKOLLI-IDKOLLI        TO SAVE-IDKOLLI-NEXT                  
072600                                                                          
072700       ELSE                                                               
072800         MOVE SAVE-IDPRODNR-ENTER   TO SAVE-IDPRODNR-NEXT                 
072900         MOVE SAVE-IDKOLLI-ENTER    TO SAVE-IDKOLLI-NEXT                  
073000       END-IF                                                             
073100         PERFORM UNTIL SEGMENT-MISSING                                    
073200                                                                          
073300           IF  DIST79-DEALER-PRICE                                        
073400               MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                 
073500               MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL             
073600               PERFORM S02-CONVERT-TO-SEK                                 
073700           ELSE                                                           
073800              MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                
073900              MOVE SKOLLI-SUORDV-LOCPREL                                  
074000                                       TO W-SUORDV-LOCPREL-SEK            
074100           END-IF                                                         
074200           COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +             
074300                                       W-SUORDV-LOCPREL-SEK +             
074400                                       SKOLLI-SUORDV                      
074500           MOVE SKOLLI-SUORDV-EXP      TO W-SUORDV-TOT-EXP                
074600           COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                  
074700           COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                     
074800                                     SKOLLI-VKORDBTO-KOLLI                
074900           COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                     
075000                                     SKOLLI-VLORDBTO-KOLLI                
075100           COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                     
075200                                     SKOLLI-VKORDNTO-KOLLI                
075300           COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                    
075400                                     W-SUORDV-TOT-SEK                     
075501           COMPUTE W-SUORDV-SKEPPN-EXP = W-SUORDV-SKEPPN-EXP +            
075601                                     W-SUORDV-TOT-EXP                     
075700           COMPUTE W-SUORDV-LOCPREL-SKP                                   
075800                                   = W-SUORDV-LOCPREL-SKP +               
075900                                     W-SUORDV-LOCPREL-SEK                 
076000           PERFORM IMS-GNP-WDE121                                         
076100         END-PERFORM                                                      
076200                                                                          
076300       IF W-KVKOLLI-SKP           > ZERO                                  
076400          MOVE W-KVKOLLI-SKP     TO RESP-KVKOLLI-SKP                      
076500                                    SAVE-KVKOLLI-SKP                      
076600       ELSE                                                               
076700          MOVE ZERO              TO SAVE-KVKOLLI-SKP                      
076800       END-IF                                                             
076900       IF W-VKORDBTO-SKP          > ZERO                                  
077000          MOVE W-VKORDBTO-SKP    TO RESP-VKORDBTO-SKP                     
077100                                    SAVE-VKORDBTO-SKP                     
077200       ELSE                                                               
077300          MOVE ZERO              TO SAVE-VKORDBTO-SKP                     
077400       END-IF                                                             
077500       IF W-VLORDBTO-SKP         > ZERO                                   
077600          MOVE W-VLORDBTO-SKP   TO RESP-VLORDBTO-SKP                      
077700                                   SAVE-VLORDBTO-SKP                      
077800       ELSE                                                               
077900          MOVE ZERO             TO SAVE-VLORDBTO-SKP                      
078000       END-IF                                                             
078100                                                                          
078200       IF W-VKORDNTO-SKP         > ZERO                                   
078300          MOVE W-VKORDNTO-SKP   TO RESP-VKORDNTO-SKP                      
078400                                   SAVE-VKORDNTO-SKP                      
078500       ELSE                                                               
078600          MOVE ZERO             TO SAVE-VKORDNTO-SKP                      
078700       END-IF                                                             
078800                                                                          
078901       MOVE ZERO             TO SAVE-SUORDV-SKEPPN                        
079001                                SAVE-SUORDV-SKEPPN-EXP                    
079100       IF W-SUORDV-SKEPPN-EXP NUMERIC AND                                 
079201          W-SUORDV-SKEPPN-EXP > ZERO                                      
079301          MOVE W-SUORDV-SKEPPN-EXP TO RESP-SUORDV-SKEPPN                  
079401                                      SAVE-SUORDV-SKEPPN-EXP              
079501       ELSE                                                               
079601          IF W-SUORDV-SKEPPN  NUMERIC                                     
079700             MOVE W-SUORDV-SKEPPN TO RESP-SUORDV-SKEPPN                   
079800                                      SAVE-SUORDV-SKEPPN                  
079900          END-IF                                                          
080000       END-IF                                                             
080101                                                                          
080200       IF W-SUORDV-LOCPREL-SKP  > ZERO                                    
080300         MOVE '*'               TO RESP-TEASTRIX-TRP                      
080400         MOVE W-SUORDV-LOCPREL-SKP                                        
080500                                TO SAVE-SUORDV-LOCPREL-SKP                
080600       ELSE                                                               
080700         MOVE SPACE             TO RESP-TEASTRIX-TRP                      
080800         MOVE ZERO              TO SAVE-SUORDV-LOCPREL-SKP                
080900       END-IF                                                             
081000                                                                          
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400                                                                          
081500 FB-READ-BEFORE-SHIPMENT SECTION.                                         
081600                                                                          
081700       PERFORM IMS-GU-WDE611-CSEQ                                         
081800                                                                          
081900     IF SEGMENT-MISSING                                                   
082000        MOVE '025'                  TO RESP-IDMSG-ERROR                   
082100        MOVE 'IDKOLLI'              TO RESP-IDELMT-ERROR                  
082200        MOVE W-WDE611-IDTRPTNR-MIN  TO SAVE-IDTRPTNR-ENTER                
082300        MOVE W-WDE611-DARFS-MIN     TO SAVE-DARFS-ENTER                   
082400        MOVE W-WDE611-ADCLGEO-MIN   TO SAVE-ADCLGEO-ENTER                 
082500        MOVE W-WDE611-ADFLOMR-MIN   TO SAVE-ADFLOMR-ENTER                 
082600        MOVE W-WDE611-ADRUTNIV-MIN  TO SAVE-ADRUTNIV-ENTER                
082700        MOVE W-WDE611-ADVMODUL-MIN  TO SAVE-ADVMODUL-ENTER                
082800        MOVE ZERO                   TO SAVE-IDPRODNR-ENTER                
082900                                       SAVE-IDKOLLI-ENTER                 
083000                                       SAVE-KVKOLLI-SKP                   
083100                                       SAVE-VKORDBTO-SKP                  
083200                                       SAVE-VLORDBTO-SKP                  
083300                                       SAVE-VKORDNTO-SKP                  
083400                                       SAVE-SUORDV-SKEPPN                 
083501                                       SAVE-SUORDV-SKEPPN-EXP             
083600                                       SAVE-SUORDV-LOCPREL-SKP            
083700     ELSE                                                                 
083800*      -- POSITION FOR READING DATA TO UPPERMOST LINE                     
083900*      -- (NOT NECESSARY IF -MIN KEYS ARE DIRECTLY USED IN SSA)           
084000                                                                          
084100       MOVE +1                      TO INDX                               
084200       IF SEGMENT-FOUND                                                   
084300         MOVE KOLLI-IDTRPTNR        TO SAVE-IDTRPTNR-ENTER                
084400         MOVE KOLLI-DARFS           TO SAVE-DARFS-ENTER                   
084500         MOVE KOLLI-ADCLGEO         TO SAVE-ADCLGEO-ENTER                 
084600         MOVE KOLLI-ADFLOMR         TO SAVE-ADFLOMR-ENTER                 
084700         MOVE KOLLI-ADRUTNIV        TO SAVE-ADRUTNIV-ENTER                
084800         MOVE KOLLI-ADVMODUL        TO SAVE-ADVMODUL-ENTER                
084900         MOVE KOLLI-IDKOLLI         TO SAVE-TRP-IDKOLLI-ENTER             
085000         MOVE ZERO                  TO SAVE-IDPRODNR-ENTER                
085100                                       SAVE-IDKOLLI-ENTER                 
085200       ELSE                                                               
085300         MOVE W-WDE611-IDTRPTNR-MIN TO SAVE-IDTRPTNR-ENTER                
085400         MOVE W-WDE611-DARFS-MIN    TO SAVE-DARFS-ENTER                   
085500         MOVE W-WDE611-ADCLGEO-MIN  TO SAVE-ADCLGEO-ENTER                 
085600         MOVE W-WDE611-ADFLOMR-MIN  TO SAVE-ADFLOMR-ENTER                 
085700         MOVE W-WDE611-ADRUTNIV-MIN TO SAVE-ADRUTNIV-ENTER                
085800         MOVE W-WDE611-ADVMODUL-MIN TO SAVE-ADVMODUL-ENTER                
085900         MOVE ZERO                  TO SAVE-IDPRODNR-ENTER                
086000                                       SAVE-IDKOLLI-ENTER                 
086100       END-IF                                                             
086200                                                                          
086300       PERFORM UNTIL INDX > MAX-INDX                                      
086400         IF SEGMENT-FOUND                                                 
086500           MOVE KOLLI-IDDISTR        TO DIST79-IDDISTR                    
086600           IF  DIST79-DEALER-PRICE                                        
086700                 IF INDX = 1                                              
086800                    MOVE KOLLI-KDVALISO                                   
086901                                        TO CURR-KDVALISO-ROW              
087000                    MOVE KOLLI-IDDISTR  TO W-WDB201-IDDISTR               
087100                                           W-WDB201-IDDISTR-MIN           
087200                                           W-WDB201-IDDISTR-MAX           
087300                    MOVE KOLLI-IDKUNDNR TO W-WDB201-IDKUNDNR              
087400                    PERFORM S06-FIND-PRKURS-FOR-FB                        
087500                 END-IF                                                   
087600              MOVE KOLLI-SUORDV-LOC     TO W-SUORDV-LOC                   
087700              MOVE KOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL               
087800              PERFORM S02-CONVERT-TO-SEK                                  
087900           ELSE                                                           
088000              MOVE KOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                 
088100              MOVE KOLLI-SUORDV-LOCPREL                                   
088200                                       TO W-SUORDV-LOCPREL-SEK            
088300           END-IF                                                         
088400           MOVE KOLLI-IDKUNDNR    TO RESP-IDKUNDNR (INDX)                 
088500           PERFORM IMS-GNP-WDE601-CSEQ                                    
088600           MOVE VORD-IDPRODNR     TO W-E4-IDPRODNR-MIN                    
088700                                     W-E4-IDPRODNR-MAX                    
088800           IF INDX = +1                                                   
088900             MOVE VORD-IDPRODNR   TO SAVE-TRP-IDPRODNR-ENTER              
089000           END-IF                                                         
089100           MOVE KOLLI-IDKOLLI     TO W-E4-IDKOLLI-MIN                     
089200                                     W-E4-IDKOLLI-MAX                     
089300           PERFORM IMS-GU-WDE4F1                                          
089400           IF SEGMENT-FOUND                                               
089500             MOVE SEQF-IDORDNR5   TO RESP-IDORDNR5 (INDX)                 
089600           END-IF                                                         
089700           MOVE KOLLI-IDKOLLI     TO RESP-IDKOLLI (INDX)                  
089800           MOVE KOLLI-IDTULL      TO RESP-IDTULL (INDX)                   
089900           IF  KOLLI-IDPSN (1)     > ZERO                                 
090000               MOVE   'Y'         TO RESP-FLFARLIG (INDX)                 
090100           END-IF                                                         
090200           MOVE KOLLI-DIKOLLIL    TO RESP-DIKOLLIL (INDX)                 
090300           MOVE KOLLI-DIKOLLIB    TO RESP-DIKOLLIB (INDX)                 
090400           MOVE KOLLI-DIKOLLIH    TO RESP-DIKOLLIH (INDX)                 
090500           MOVE KOLLI-VKORDBTO-KOLLI                                      
090600                                  TO RESP-VKORDBTO-KOLLI (INDX)           
090700           MOVE KOLLI-VLORDBTO-KOLLI                                      
090800                                  TO RESP-VLORDBTO-KOLLI (INDX)           
090900           MOVE KOLLI-VKORDNTO-KOLLI                                      
091000                                  TO RESP-VKORDNTO-KOLLI (INDX)           
091100           MOVE  ZERO   TO   W-SUORDV-TOT-SEK                             
091201                             W-SUORDV-TOT-EXP                             
091300           COMPUTE W-SUORDV-TOT-SEK = KOLLI-SUORDV-KOLLI +                
091400                                      W-SUORDV-LOC-SEK   +                
091500                                      W-SUORDV-LOCPREL-SEK                
091601           MOVE KOLLI-SUORDV-KLI-EXP  TO W-SUORDV-TOT-EXP                 
091701                                                                          
091801           IF W-SUORDV-TOT-EXP > ZERO                                     
091901             MOVE W-SUORDV-TOT-EXP TO RESP-SUORDV-KOLLI (INDX)            
092001           ELSE                                                           
092100             MOVE W-SUORDV-TOT-SEK TO RESP-SUORDV-KOLLI (INDX)            
092200           END-IF                                                         
092301                                                                          
092400           IF W-SUORDV-LOCPREL-SEK   > ZERO                               
092500             MOVE '*'             TO RESP-TEASTRIX-KLI (INDX)             
092600           ELSE                                                           
092700             MOVE ' '             TO RESP-TEASTRIX-KLI (INDX)             
092800           END-IF                                                         
092900                                                                          
093000           ADD 1 TO WS-COUNT                                              
093100                                                                          
093200           MOVE WS-COUNT TO RESP-KVRADER                                  
093300               COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1              
093400               COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                 
093500                                         KOLLI-VKORDBTO-KOLLI             
093600               COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                 
093700                                         KOLLI-VLORDBTO-KOLLI             
093800               COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                 
093900                                         KOLLI-VKORDNTO-KOLLI             
094000               COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                
094100                                         W-SUORDV-TOT-SEK                 
094201               COMPUTE W-SUORDV-SKEPPN-EXP = W-SUORDV-SKEPPN-EXP +        
094301                                         W-SUORDV-TOT-EXP                 
094400               COMPUTE W-SUORDV-LOCPREL-SKP                               
094500                                       = W-SUORDV-LOCPREL-SKP +           
094600                                         W-SUORDV-LOCPREL-SEK             
094700           PERFORM IMS-GN-WDE611-CSEQ                                     
094800         END-IF                                                           
094900         ADD 1 TO INDX                                                    
095000       END-PERFORM                                                        
095100                                                                          
095200       IF SEGMENT-FOUND                                                   
095300                                                                          
095400         IF INDX > MAX-INDX                                               
095500           PERFORM IMS-GNP-WDE601-CSEQ                                    
095600         END-IF                                                           
095700                                                                          
095800         MOVE KOLLI-IDTRPTNR TO SAVE-IDTRPTNR-NEXT                        
095900         MOVE KOLLI-DARFS    TO SAVE-DARFS-NEXT                           
096000         MOVE KOLLI-ADCLGEO  TO SAVE-ADCLGEO-NEXT                         
096100         MOVE KOLLI-ADFLOMR  TO SAVE-ADFLOMR-NEXT                         
096200         MOVE KOLLI-ADRUTNIV TO SAVE-ADRUTNIV-NEXT                        
096300         MOVE KOLLI-ADVMODUL TO SAVE-ADVMODUL-NEXT                        
096400         MOVE KOLLI-IDKOLLI  TO SAVE-TRP-IDKOLLI-NEXT                     
096500         MOVE VORD-IDPRODNR  TO SAVE-TRP-IDPRODNR-NEXT                    
096600                                                                          
096700       ELSE                                                               
096800         MOVE SAVE-IDTRPTNR-ENTER TO  SAVE-IDTRPTNR-NEXT                  
096900         MOVE SAVE-DARFS-ENTER    TO  SAVE-DARFS-NEXT                     
097000         MOVE SAVE-ADCLGEO-ENTER  TO  SAVE-ADCLGEO-NEXT                   
097100         MOVE SAVE-ADFLOMR-ENTER  TO  SAVE-ADFLOMR-NEXT                   
097200         MOVE SAVE-ADRUTNIV-ENTER TO  SAVE-ADRUTNIV-NEXT                  
097300         MOVE SAVE-ADVMODUL-ENTER TO  SAVE-ADVMODUL-NEXT                  
097400         MOVE SAVE-TRP-IDPRODNR-ENTER TO SAVE-TRP-IDPRODNR-NEXT           
097500         MOVE SAVE-TRP-IDKOLLI-ENTER TO SAVE-TRP-IDKOLLI-NEXT             
097600                                                                          
097700       END-IF                                                             
097800         PERFORM UNTIL SEGMENT-MISSING                                    
097900                                                                          
098000           IF DIST79-DEALER-PRICE                                         
098100              MOVE KOLLI-SUORDV-LOC      TO W-SUORDV-LOC                  
098200              MOVE KOLLI-SUORDV-LOCPREL  TO W-SUORDV-LOCPREL              
098300              PERFORM S02-CONVERT-TO-SEK                                  
098400           ELSE                                                           
098500              MOVE KOLLI-SUORDV-LOC      TO W-SUORDV-LOC-SEK              
098600              MOVE KOLLI-SUORDV-LOCPREL  TO W-SUORDV-LOCPREL-SEK          
098700           END-IF                                                         
098800           COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +             
098900                                       W-SUORDV-LOCPREL-SEK +             
099000                                       KOLLI-SUORDV-KOLLI                 
099100           MOVE KOLLI-SUORDV-KLI-EXP TO W-SUORDV-TOT-EXP                  
099201                                                                          
099300           COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                  
099400           COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                     
099500                                     KOLLI-VKORDBTO-KOLLI                 
099600           COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                     
099700                                     KOLLI-VLORDBTO-KOLLI                 
099800           COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                     
099900                                     KOLLI-VKORDNTO-KOLLI                 
100000           COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                    
100100                                     W-SUORDV-TOT-SEK                     
100201           COMPUTE W-SUORDV-SKEPPN-EXP = W-SUORDV-SKEPPN-EXP +            
100301                                     W-SUORDV-TOT-EXP                     
100400           COMPUTE W-SUORDV-LOCPREL-SKP                                   
100500                                   = W-SUORDV-LOCPREL-SKP +               
100600                                     W-SUORDV-LOCPREL-SEK                 
100700           PERFORM IMS-GN-WDE611-CSEQ                                     
100800         END-PERFORM                                                      
100900                                                                          
101000       IF W-KVKOLLI-SKP           > ZERO                                  
101100          MOVE W-KVKOLLI-SKP     TO RESP-KVKOLLI-SKP                      
101200                                    SAVE-KVKOLLI-SKP                      
101300       ELSE                                                               
101400          MOVE ZERO              TO SAVE-KVKOLLI-SKP                      
101500       END-IF                                                             
101600                                                                          
101700       IF W-VKORDBTO-SKP          > ZERO                                  
101800          MOVE W-VKORDBTO-SKP    TO RESP-VKORDBTO-SKP                     
101900                                    SAVE-VKORDBTO-SKP                     
102000       ELSE                                                               
102100          MOVE ZERO              TO SAVE-VKORDBTO-SKP                     
102200       END-IF                                                             
102300       IF W-VLORDBTO-SKP         > ZERO                                   
102400          MOVE W-VLORDBTO-SKP   TO RESP-VLORDBTO-SKP                      
102500                                   SAVE-VLORDBTO-SKP                      
102600       ELSE                                                               
102700          MOVE ZERO             TO SAVE-VLORDBTO-SKP                      
102800       END-IF                                                             
102900       IF W-VKORDNTO-SKP         > ZERO                                   
103000          MOVE W-VKORDNTO-SKP   TO RESP-VKORDNTO-SKP                      
103100                                   SAVE-VKORDNTO-SKP                      
103200       ELSE                                                               
103300          MOVE ZERO             TO SAVE-VKORDNTO-SKP                      
103400       END-IF                                                             
103500       IF W-SUORDV-SKEPPN-EXP NUMERIC AND                                 
103601          W-SUORDV-SKEPPN-EXP > ZERO                                      
103701          MOVE W-SUORDV-SKEPPN-EXP  TO RESP-SUORDV-SKEPPN                 
103801                                     SAVE-SUORDV-SKEPPN-EXP               
103901       ELSE                                                               
104001         IF W-SUORDV-SKEPPN   NUMERIC                                     
104100            MOVE W-SUORDV-SKEPPN TO RESP-SUORDV-SKEPPN                    
104200                                     SAVE-SUORDV-SKEPPN                   
104300         ELSE                                                             
104400            MOVE ZERO           TO SAVE-SUORDV-SKEPPN                     
104500         END-IF                                                           
104601       END-IF                                                             
104700       IF W-SUORDV-LOCPREL-SKP  > ZERO                                    
104800         MOVE '*'               TO RESP-TEASTRIX-TRP                      
104900         MOVE W-SUORDV-LOCPREL-SKP                                        
105000                                TO SAVE-SUORDV-LOCPREL-SKP                
105100       ELSE                                                               
105200         MOVE ZERO              TO SAVE-SUORDV-LOCPREL-SKP                
105300       END-IF                                                             
105400                                                                          
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800                                                                          
105900 FC-READ-SUM-PER-SHIPMENT SECTION.                                        
106000                                                                          
106100     PERFORM IMS-GHU-WDE101                                               
106200     IF SEGMENT-MISSING                                                   
106300       MOVE '025'            TO RESP-IDMSG-ERROR                          
106400       MOVE 'IDTRP'          TO RESP-IDELMT-ERROR                         
106500     ELSE                                                                 
106600       PERFORM IMS-GNP-WDE111                                             
106700       IF SEGMENT-MISSING                                                 
106800         MOVE '025'            TO RESP-IDMSG-ERROR                        
106900         MOVE 'IDTRP'          TO RESP-IDELMT-ERROR                       
107000       ELSE                                                               
107100         MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN                    
107200         MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN                   
107300         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
107400         PERFORM S01-FIND-PRKURS                                          
107500         MOVE +1 TO INDX                                                  
107600         PERFORM UNTIL INDX > MAX-INDX                                    
107700           IF SEGMENT-FOUND                                               
107800             IF INDX = +1                                                 
107900               MOVE SKOLLI-IDDISTR   TO SAVE-IDDISTR-ENTER                
108000                                        SAVE-IDDISTR-NEXT                 
108100               MOVE SKOLLI-IDKUNDNR  TO SAVE-IDKUNDNR-ENTER               
108200                                        SAVE-IDKUNDNR-NEXT                
108300               MOVE SKOLLI-IDPRODNR  TO SAVE-IDPRODNR-ENTER               
108400                                        SAVE-IDPRODNR-NEXT                
108500               MOVE SKOLLI-IDKOLLI   TO SAVE-IDKOLLI-ENTER                
108600                                        SAVE-IDKOLLI-NEXT                 
108700             END-IF                                                       
108800                                                                          
108900             MOVE SKOLLI-IDKUNDNR TO RESP-IDKUNDNR (INDX)                 
109000             MOVE SKOLLI-IDORDNR7 TO RESP-IDORDNR5 (INDX)                 
109100             MOVE SKOLLI-IDKOLLI  TO RESP-IDKOLLI (INDX)                  
109200                                                                          
109300             MOVE SKOLLI-DIKOLLIL TO RESP-DIKOLLIL (INDX)                 
109400             MOVE SKOLLI-DIKOLLIB TO RESP-DIKOLLIB (INDX)                 
109500             MOVE SKOLLI-DIKOLLIH TO RESP-DIKOLLIH (INDX)                 
109600             MOVE SKOLLI-VKORDBTO-KOLLI                                   
109700                                    TO RESP-VKORDBTO-KOLLI (INDX)         
109800             MOVE SKOLLI-VLORDBTO-KOLLI                                   
109900                                    TO RESP-VLORDBTO-KOLLI (INDX)         
110000             MOVE SKOLLI-VKORDNTO-KOLLI                                   
110100                                    TO RESP-VKORDNTO-KOLLI (INDX)         
110200             MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                 
110300             MOVE SKOLLI-SUORDV-LOCPREL                                   
110400                                       TO W-SUORDV-LOCPREL-SEK            
110500             COMPUTE W-SUORDV-TOT-SEK = SKOLLI-SUORDV  +                  
110600                                        W-SUORDV-LOC-SEK +                
110700                                        W-SUORDV-LOCPREL-SEK              
110801             MOVE SKOLLI-SUORDV-EXP  TO W-SUORDV-TOT-EXP                  
110901                                                                          
111000             IF W-SUORDV-TOT-EXP > ZERO                                   
111101               MOVE W-SUORDV-TOT-EXP TO RESP-SUORDV-KOLLI (INDX)          
111201             ELSE                                                         
111301               MOVE W-SUORDV-TOT-SEK TO RESP-SUORDV-KOLLI (INDX)          
111401             END-IF                                                       
111500                                                                          
111600             IF W-SUORDV-LOCPREL-SEK > ZERO                               
111700               MOVE '*'           TO RESP-TEASTRIX-KLI (INDX)             
111800             ELSE                                                         
111900               MOVE ' '           TO RESP-TEASTRIX-KLI (INDX)             
112000             END-IF                                                       
112100                                                                          
112200             MOVE SKOLLI-IDPRODNR TO W-WDE601-IDPRODNR                    
112300             MOVE SKOLLI-IDKOLLI  TO W-WDE611-IDKOLLI                     
112400             PERFORM IMS-GU-WDE611                                        
112500             IF SEGMENT-FOUND                                             
112600                MOVE KOLLI-IDTULL TO RESP-IDTULL (INDX)                   
112700                IF KOLLI-IDPSN (1) > ZERO                                 
112800                    MOVE 'Y'      TO RESP-FLFARLIG (INDX)                 
112900                END-IF                                                    
113000             ELSE                                                         
113100                MOVE SPACE        TO RESP-IDTULL (INDX)                   
113200                                     RESP-FLFARLIG (INDX)                 
113300             END-IF                                                       
113400                                                                          
113500             PERFORM S04-WDE121-SUM                                       
113600             PERFORM IMS-GNP-WDE121-INCL-WDE111                           
113700             ADD 1 TO INDX                                                
113800             ADD 1 TO WS-COUNT                                            
113900                                                                          
114000             MOVE WS-COUNT TO RESP-KVRADER                                
114100           ELSE                                                           
114200             PERFORM IMS-GNP-WDE111                                       
114300             IF SEGMENT-MISSING                                           
114400               PERFORM UNTIL INDX > MAX-INDX                              
114500                ADD 1 TO INDX                                             
114600               END-PERFORM                                                
114700             ELSE                                                         
114800               MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN              
114900               MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN             
115000               PERFORM IMS-GNP-WDE121-INCL-WDE111                         
115100               PERFORM S01-FIND-PRKURS                                    
115200             END-IF                                                       
115300           END-IF                                                         
115400         END-PERFORM                                                      
115500                                                                          
115600         IF SEGMENT-FOUND                                                 
115700           MOVE SKOLLI-IDDISTR   TO SAVE-IDDISTR-NEXT                     
115800           MOVE SKOLLI-IDKUNDNR  TO SAVE-IDKUNDNR-NEXT                    
115900           MOVE SKOLLI-IDPRODNR  TO SAVE-IDPRODNR-NEXT                    
116000           MOVE SKOLLI-IDKOLLI   TO SAVE-IDKOLLI-NEXT                     
116100             PERFORM FCA-WDE1-SUM                                         
116200                                                                          
116300             MOVE W-KVKOLLI-SKP   TO SAVE-KVKOLLI-SKP                     
116400             MOVE W-VKORDBTO-SKP  TO SAVE-VKORDBTO-SKP                    
116500             MOVE W-VLORDBTO-SKP  TO SAVE-VLORDBTO-SKP                    
116600             MOVE W-VKORDNTO-SKP  TO SAVE-VKORDNTO-SKP                    
116700             MOVE W-SUORDV-SKEPPN TO SAVE-SUORDV-SKEPPN                   
116801             MOVE W-SUORDV-SKEPPN-EXP TO SAVE-SUORDV-SKEPPN-EXP           
116900             MOVE W-SUORDV-LOCPREL-SKP TO SAVE-SUORDV-LOCPREL-SKP         
117000                                                                          
117100         ELSE                                                             
117200           MOVE SAVE-IDDISTR-ENTER    TO SAVE-IDDISTR-NEXT                
117300           MOVE SAVE-IDKUNDNR-ENTER   TO SAVE-IDKUNDNR-NEXT               
117400           MOVE SAVE-IDPRODNR-ENTER   TO SAVE-IDPRODNR-NEXT               
117500           MOVE SAVE-IDKOLLI-ENTER    TO SAVE-IDKOLLI-NEXT                
117600           PERFORM S05-SAVE-SUM                                           
117700         END-IF                                                           
117800                                                                          
117900       END-IF                                                             
118000         MOVE SAVE-KVKOLLI-SKP        TO RESP-KVKOLLI-SKP                 
118100         MOVE SAVE-VKORDBTO-SKP       TO RESP-VKORDBTO-SKP                
118200         MOVE SAVE-VLORDBTO-SKP       TO RESP-VLORDBTO-SKP                
118300         MOVE SAVE-VKORDNTO-SKP       TO RESP-VKORDNTO-SKP                
118400         IF SAVE-SUORDV-SKEPPN-EXP > ZERO                                 
118501           MOVE SAVE-SUORDV-SKEPPN-EXP TO RESP-SUORDV-SKEPPN              
118601         ELSE                                                             
118701           MOVE SAVE-SUORDV-SKEPPN    TO RESP-SUORDV-SKEPPN               
118801         END-IF                                                           
118900     END-IF                                                               
119000     .                                                                    
119100     EJECT                                                                
119200 FCA-WDE1-SUM SECTION.                                                    
119300     PERFORM UNTIL SEGMENT-MISSING                                        
119400       PERFORM UNTIL SEGMENT-MISSING                                      
119500         PERFORM S04-WDE121-SUM                                           
119600         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
119700       END-PERFORM                                                        
119800       PERFORM IMS-GNP-WDE111                                             
119900       IF SEGMENT-FOUND                                                   
120000           MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN                  
120100           MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN                 
120200         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
120300         PERFORM S01-FIND-PRKURS                                          
120400       END-IF                                                             
120500     END-PERFORM                                                          
120600     .                                                                    
120700     EJECT                                                                
120800                                                                          
120900 S01-FIND-PRKURS SECTION.                                                 
121000                                                                          
121101     MOVE SKOLLI-KDVALISO TO CURR-KDVALISO-ROW                            
121200     MOVE SGMT-IDDISTR  TO W-WDB201-IDDISTR                               
121300                           W-WDB201-IDDISTR-MIN                           
121400                           W-WDB201-IDDISTR-MAX                           
121500     MOVE SGMT-IDKUNDNR TO W-WDB201-IDKUNDNR                              
121600                                                                          
121701** I/P  CURR-KDVALISO-ROW     W-WDB201-IDDISTR   W-WDB201-IDKUNDNR        
121800**      W-WDB201-IDDISTR-MIN  W-WDB201-IDDISTR-MAX                        
121900** O/P  SAVE-PRKURS                                                       
122000                                                                          
122101     IF CURR-KDVALISO-ROW       > SPACE                                   
122200       CONTINUE                                                           
122300     ELSE                                                                 
122400       PERFORM IMS-GU-WDB201-UNIQ                                         
122500       IF SEGMENT-FOUND                                                   
122600         CONTINUE                                                         
122700       ELSE                                                               
122800         PERFORM IMS-GU-WDB201                                            
122900       END-IF                                                             
123000       MOVE GMT-IDPARTNR       TO W-WDB101-IDPARTNR                       
123100       MOVE WS-DCS-IDFTG       TO W-WDB101-IDFTG                          
123200       PERFORM IMS-GU-WDB101                                              
123301       MOVE BET-KDVALISO       TO CURR-KDVALISO-ROW                       
123400     END-IF                                                               
123501     MOVE W-DATE-AAMM          TO CURR-TIAAMM                             
123601     MOVE WS-KDVALISO-HUV      TO CURR-KDVALISO-HUV                       
123701     MOVE 'M'                  TO CURR-KDVALTYP                           
123801                                                                          
123901     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
124001     IF CURR-KDSVAR = ' '                                                 
124101        MOVE CURR-PRKURS-NEW   TO SAVE-PRKURS                             
124201     ELSE                                                                 
124301        MOVE 1                 TO SAVE-PRKURS                             
124401     END-IF                                                               
124500     .                                                                    
124600     EJECT                                                                
124700                                                                          
124800 S02-CONVERT-TO-SEK SECTION.                                              
124900** I/P  SAVE-PRKURS  W-SUORDV-LOC        W-SUORDV-LOCPREL                 
125000** O/P               W-SUORDV-LOC-SEK    W-SUORDV-LOCPREL-SEK             
125100                                                                          
125200     MOVE SAVE-PRKURS        TO EXCH-PRKURS                               
125300**   +1 KDCALL = LOCAL CURRENCY TO SEK                                    
125400     MOVE +1                 TO EXCH-KDCALL                               
125500     MOVE W-SUORDV-LOC       TO EXCH-SUORDV-IN                            
125600     MOVE +0                 TO EXCH-PRARTNTO-IN                          
125700     CALL W411EXCH USING EXCH-W411EXCH                                    
125800     MOVE EXCH-SUORDV-UT     TO W-SUORDV-LOC-SEK                          
125900                                                                          
126000     MOVE +1                 TO EXCH-KDCALL                               
126100     MOVE W-SUORDV-LOCPREL   TO EXCH-SUORDV-IN                            
126200     MOVE +0                 TO EXCH-PRARTNTO-IN                          
126300     CALL W411EXCH USING EXCH-W411EXCH                                    
126400     MOVE EXCH-SUORDV-UT     TO W-SUORDV-LOCPREL-SEK                      
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800 S03-NOLLA-SAVE-TRP  SECTION.                                             
126900     MOVE ZERO                   TO SAVE-IDTRPTNR-ENTER                   
127000                                    SAVE-DARFS-ENTER                      
127100                                    SAVE-ADKOLLI-ENTER                    
127200                                    SAVE-ADFLOMR-ENTER                    
127300                                    SAVE-ADRUTNIV-ENTER                   
127400                                    SAVE-ADVMODUL-ENTER                   
127500                                    SAVE-KVKOLLI-SKP                      
127600                                    SAVE-VKORDBTO-SKP                     
127700                                    SAVE-VLORDBTO-SKP                     
127800                                    SAVE-VKORDNTO-SKP                     
127900                                    SAVE-SUORDV-SKEPPN                    
128001                                    SAVE-SUORDV-SKEPPN-EXP                
128100                                    SAVE-SUORDV-LOCPREL-SKP               
128200     MOVE SPACE                  TO SAVE-IDDC-ENTER                       
128300                                    SAVE-ADFLGEO-ENTER                    
128400     MOVE ZERO                   TO SAVE-TRP-IDPRODNR-ENTER               
128500                                    SAVE-TRP-IDKOLLI-ENTER                
128600     .                                                                    
128700     EJECT                                                                
128800                                                                          
128900 S03-NOLLA-SAVE-SHIPM  SECTION.                                           
129000     MOVE ZERO                   TO SAVE-IDPRODNR-ENTER                   
129100                                    SAVE-IDPRODNR-NEXT                    
129200                                    SAVE-IDKUNDNR-ENTER                   
129300                                    SAVE-IDKUNDNR-NEXT                    
129400                                    SAVE-IDKOLLI-ENTER                    
129500                                    SAVE-IDKOLLI-NEXT                     
129600                                    SAVE-IDDISTR-ENTER                    
129700                                    SAVE-IDDISTR-NEXT                     
129800     .                                                                    
129900     EJECT                                                                
130000                                                                          
130100 S04-WDE121-SUM SECTION.                                                  
130200                                                                          
130300     IF  DIST79-DEALER-PRICE                                              
130400         MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                       
130500         MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL                   
130600         PERFORM S02-CONVERT-TO-SEK                                       
130700     ELSE                                                                 
130800        MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                      
130900        MOVE SKOLLI-SUORDV-LOCPREL                                        
131000                                 TO W-SUORDV-LOCPREL-SEK                  
131100     END-IF                                                               
131200     COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +                   
131300                                 W-SUORDV-LOCPREL-SEK +                   
131400                                 SKOLLI-SUORDV                            
131501     MOVE SKOLLI-SUORDV-EXP  TO W-SUORDV-TOT-EXP                          
131600                                                                          
131700     COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                        
131800     COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                           
131900                               SKOLLI-VKORDBTO-KOLLI                      
132000     COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                           
132100                               SKOLLI-VLORDBTO-KOLLI                      
132200     COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                           
132300                               SKOLLI-VKORDNTO-KOLLI                      
132400     COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                          
132500                               W-SUORDV-TOT-SEK                           
132601     COMPUTE W-SUORDV-SKEPPN-EXP = W-SUORDV-SKEPPN-EXP +                  
132701                               W-SUORDV-TOT-EXP                           
132800     COMPUTE W-SUORDV-LOCPREL-SKP                                         
132900                             = W-SUORDV-LOCPREL-SKP +                     
133000                               W-SUORDV-LOCPREL-SEK                       
133100     .                                                                    
133200     EJECT                                                                
133300 S05-SAVE-SUM SECTION.                                                    
133400                                                                          
133500     IF W-KVKOLLI-SKP             > ZERO                                  
133600        MOVE W-KVKOLLI-SKP       TO RESP-KVKOLLI-SKP                      
133700                                  SAVE-KVKOLLI-SKP                        
133800     ELSE                                                                 
133900        MOVE ZERO                TO SAVE-KVKOLLI-SKP                      
134000     END-IF                                                               
134100     IF W-VKORDBTO-SKP            > ZERO                                  
134200        MOVE W-VKORDBTO-SKP      TO RESP-VKORDBTO-SKP                     
134300                                  SAVE-VKORDBTO-SKP                       
134400     ELSE                                                                 
134500        MOVE ZERO                TO SAVE-VKORDBTO-SKP                     
134600     END-IF                                                               
134700     IF W-VLORDBTO-SKP           > ZERO                                   
134800        MOVE W-VLORDBTO-SKP     TO RESP-VLORDBTO-SKP                      
134900                                 SAVE-VLORDBTO-SKP                        
135000     ELSE                                                                 
135100        MOVE ZERO               TO SAVE-VLORDBTO-SKP                      
135200     END-IF                                                               
135300                                                                          
135400     IF W-VKORDNTO-SKP           > ZERO                                   
135500        MOVE W-VKORDNTO-SKP     TO RESP-VKORDNTO-SKP                      
135600                                 SAVE-VKORDNTO-SKP                        
135700     ELSE                                                                 
135800        MOVE ZERO               TO SAVE-VKORDNTO-SKP                      
135900     END-IF                                                               
136000                                                                          
136101     MOVE ZERO                TO SAVE-SUORDV-SKEPPN                       
136201                                 SAVE-SUORDV-SKEPPN-EXP                   
136300     IF W-SUORDV-SKEPPN-EXP   NUMERIC AND                                 
136401        W-SUORDV-SKEPPN-EXP > ZERO                                        
136501        MOVE W-SUORDV-SKEPPN-EXP TO RESP-SUORDV-SKEPPN                    
136601                                 SAVE-SUORDV-SKEPPN-EXP                   
136701     ELSE                                                                 
136801       IF W-SUORDV-SKEPPN     NUMERIC                                     
136900          MOVE W-SUORDV-SKEPPN  TO RESP-SUORDV-SKEPPN                     
137000                                   SAVE-SUORDV-SKEPPN                     
137100       END-IF                                                             
137201     END-IF                                                               
137300                                                                          
137400     IF W-SUORDV-LOCPREL-SKP    > ZERO                                    
137500       MOVE '*'                 TO RESP-TEASTRIX-TRP                      
137600       MOVE W-SUORDV-LOCPREL-SKP                                          
137700                              TO SAVE-SUORDV-LOCPREL-SKP                  
137800     ELSE                                                                 
137900       MOVE SPACE               TO RESP-TEASTRIX-TRP                      
138000       MOVE ZERO                TO SAVE-SUORDV-LOCPREL-SKP                
138100     END-IF                                                               
138200     .                                                                    
138300     EJECT                                                                
138400                                                                          
138500 S06-FIND-PRKURS-FOR-FB SECTION.                                          
138600                                                                          
138701** I/P  CURR-KDVALISO-ROW     W-WDB201-IDDISTR   W-WDB201-IDKUNDNR        
138800**      W-WDB201-IDDISTR-MIN  W-WDB201-IDDISTR-MAX                        
138900** O/P  SAVE-PRKURS                                                       
139000                                                                          
139101     IF CURR-KDVALISO-ROW       > SPACE                                   
139200       CONTINUE                                                           
139300     ELSE                                                                 
139400       PERFORM IMS-GU-WDB201-UNIQ                                         
139500       IF SEGMENT-FOUND                                                   
139600         CONTINUE                                                         
139700       ELSE                                                               
139800         PERFORM IMS-GU-WDB201                                            
139900       END-IF                                                             
140000       MOVE GMT-IDPARTNR       TO W-WDB101-IDPARTNR                       
140100       MOVE WS-DCS-IDFTG       TO W-WDB101-IDFTG                          
140200       PERFORM IMS-GU-WDB101                                              
140301       MOVE BET-KDVALISO       TO CURR-KDVALISO-ROW                       
140400     END-IF                                                               
140500                                                                          
140601     MOVE W-DATE-AAMM          TO CURR-TIAAMM                             
140701     MOVE WS-KDVALISO-HUV      TO CURR-KDVALISO-HUV                       
140801     MOVE 'M'                  TO CURR-KDVALTYP                           
140901                                                                          
141001     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
141101     IF CURR-KDSVAR = ' '                                                 
141201        MOVE CURR-PRKURS-NEW   TO SAVE-PRKURS                             
141301     ELSE                                                                 
141401        MOVE 1                 TO SAVE-PRKURS                             
141501     END-IF                                                               
141800     .                                                                    
142000     EJECT                                                                
142100*    --- DISPATCHER SECTIONS                                              
142200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
142300                                                                          
142400     MOVE 'GETARG'               TO SUB-KDFUNC                            
142500     MOVE 'CARPARTS.LDC.BOOKINGINFO'        TO SUB-ADDISPABS              
142600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
142700                                                                          
142800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
142900                                                                          
143000     IF SUB-KDRC > 0                                                      
143100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
143200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
143300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
143400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
143500     END-IF                                                               
143600     .                                                                    
143700     SKIP3                                                                
143800 S02-RETURN-RESPONSE SECTION.                                             
143900                                                                          
144000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
144100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
144200                                                                          
144300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
144400                                                                          
144500     IF SUB-KDRC > 0                                                      
144600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
144700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
144800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
144900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
145000     END-IF                                                               
145100     .                                                                    
145200     EJECT                                                                
145300 IMS-GHU-WDE101 SECTION.                                                  
145400                                                                          
145500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
145600          DELIMITED BY SIZE INTO SSA1                                     
145700     MOVE '  GE' TO GOOD-STATUSCODES                                      
145800     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
145900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
146000     PERFORM IMS-STATUSCHECK                                              
146100     .                                                                    
146200     EJECT                                                                
146300 IMS-GU-WDE111 SECTION.                                                   
146400                                                                          
146500     STRING 'WDE101  (IDSHIPM  =' W-WDE101-IDSHIPM-X ')'                  
146600          DELIMITED BY SIZE INTO SSA1                                     
146700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X                            
146800                    '&IDDC     =' W-IDDC-X        ')'                     
146900          DELIMITED BY SIZE INTO SSA2                                     
147000     MOVE '  GE' TO GOOD-STATUSCODES                                      
147100     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
147200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
147300     PERFORM IMS-STATUSCHECK                                              
147400     .                                                                    
147500     EJECT                                                                
147600 IMS-GNP-WDE111 SECTION.                                                  
147700                                                                          
147800     STRING 'WDE101  (IDSHIPM  =' W-WDE101-IDSHIPM-X ')'                  
147900          DELIMITED BY SIZE INTO SSA1                                     
148000     STRING 'WDE111  (WDE111KY=>' W-WDE111KY-MIN                          
148100                    '&WDE111KY=<' W-WDE111KY-MAX  ')'                     
148200          DELIMITED BY SIZE INTO SSA2                                     
148300     MOVE '  GE' TO GOOD-STATUSCODES                                      
148400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
148500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
148600     PERFORM IMS-STATUSCHECK                                              
148700     .                                                                    
148800     EJECT                                                                
148900 IMS-GNP-WDE121 SECTION.                                                  
149000                                                                          
149100     STRING 'WDE121  (WDE121KY>=' W-WDE121KY-MIN                          
149200                    '&WDE121KY<=' W-WDE121KY-MAX  ')'                     
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     MOVE '  GE' TO GOOD-STATUSCODES                                      
149500     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1                   
149600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSCHECK                                              
149800     .                                                                    
149900     EJECT                                                                
150000 IMS-GNP-WDE121-INCL-WDE111 SECTION.                                      
150100                                                                          
150200     STRING 'WDE111  (WDE111KY=>' W-WDE111KY-MIN                          
150300                    '&WDE111KY=<' W-WDE111KY-MAX  ')'                     
150400                    '&IDDC     =' W-IDDC-X        ')'                     
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     STRING 'WDE121  (WDE121KY=>' W-WDE121KY-MIN                          
150700                    '&WDE121KY=<' W-WDE121KY-MAX  ')'                     
150800          DELIMITED BY SIZE INTO SSA2                                     
150900     MOVE '  GE' TO GOOD-STATUSCODES                                      
151000     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
151100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
151200     PERFORM IMS-STATUSCHECK                                              
151300     .                                                                    
151400     EJECT                                                                
151500 IMS-GU-WDE611 SECTION.                                                   
151600                                                                          
151700     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
151800          DELIMITED BY SIZE INTO SSA1                                     
151900     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
152000          DELIMITED BY SIZE INTO SSA2                                     
152100     MOVE '  GE' TO GOOD-STATUSCODES                                      
152200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
152300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
152400     PERFORM IMS-STATUSCHECK                                              
152500     .                                                                    
152600     EJECT                                                                
152700 IMS-GU-WDE611-CSEQ SECTION.                                              
152800                                                                          
152900     STRING 'WDE611  (WDE6CSEQ>=' W-WDE6CSEQ-MIN                          
153000                    '&WDE6CSEQ<=' W-WDE6CSEQ-MAX                          
153100                    '&IDDC     =' W-IDDC-X                                
153200                    '&IDDISTR  =' W-IDDISTR-X                             
153300                    '&IDKUNDNR =' W-IDKUNDNR-X  ')'                       
153400          DELIMITED BY SIZE INTO SSA1                                     
153500     MOVE '  GE' TO GOOD-STATUSCODES                                      
153600     CALL CBLTDLI USING GU WDE6CSQ-PCB DLI-IO-WDE611 SSA1                 
153700     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
153800     PERFORM IMS-STATUSCHECK                                              
153900     .                                                                    
154000     EJECT                                                                
154100 IMS-GN-WDE611-CSEQ SECTION.                                              
154200                                                                          
154300     STRING 'WDE611  (WDE6CSEQ>=' W-WDE6CSEQ-MIN                          
154400                    '&WDE6CSEQ<=' W-WDE6CSEQ-MAX                          
154500                    '&IDDC     =' W-IDDC-X                                
154600                    '&IDDISTR  =' W-IDDISTR-X                             
154700                    '&IDKUNDNR =' W-IDKUNDNR-X  ')'                       
154800          DELIMITED BY SIZE INTO SSA1                                     
154900     MOVE '  GE' TO GOOD-STATUSCODES                                      
155000     CALL CBLTDLI USING GN WDE6CSQ-PCB DLI-IO-WDE611 SSA1                 
155100     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
155200     PERFORM IMS-STATUSCHECK                                              
155300     .                                                                    
155400     EJECT                                                                
155500 IMS-GNP-WDE601-CSEQ SECTION.                                             
155600                                                                          
155700     MOVE 'WDE601  *F'        TO SSA1                                     
155800     MOVE '  ' TO GOOD-STATUSCODES                                        
155900     CALL CBLTDLI USING GNP WDE6CSQ-PCB DLI-IO-WDE601 SSA1                
156000     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
156100     PERFORM IMS-STATUSCHECK                                              
156200     .                                                                    
156300     EJECT                                                                
156400 IMS-GU-WDE4F1   SECTION.                                                 
156500                                                                          
156600     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
156700                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GE' TO GOOD-STATUSCODES                                      
157000     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
157100     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
157200     PERFORM IMS-STATUSCHECK                                              
157300     .                                                                    
157400 IMS-GU-WDB201 SECTION.                                                   
157500                                                                          
157600     STRING 'WDB201  (IDGMT   >=' W-WDB201-IDGMT-MIN-X                    
157700                    '&IDGMT   <=' W-WDB201-IDGMT-MAX-X ')'                
157800            DELIMITED BY SIZE INTO SSA1                                   
157900     MOVE '  ' TO GOOD-STATUSCODES                                        
158000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
158100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSCHECK                                              
158300     .                                                                    
158400     EJECT                                                                
158500 IMS-GU-WDB201-UNIQ SECTION.                                              
158600                                                                          
158700     STRING 'WDB201  (IDGMT    =' W-WDB201-IDGMT-X ')'                    
158800            DELIMITED BY SIZE INTO SSA1                                   
158900                                                                          
159000     MOVE '  GE' TO GOOD-STATUSCODES                                      
159100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
159200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
159300     PERFORM IMS-STATUSCHECK                                              
159400     .                                                                    
159500     EJECT                                                                
159600 IMS-GU-WDB101 SECTION.                                                   
159700                                                                          
159800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
159900          DELIMITED BY SIZE INTO SSA1                                     
160000     MOVE '  ' TO GOOD-STATUSCODES                                        
160100     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
160200     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
160300     PERFORM IMS-STATUSCHECK                                              
160400     .                                                                    
160500     EJECT                                                                
160601 IMS-GU-WDB601    SECTION.                                                
160701     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
160801          DELIMITED BY SIZE INTO SSA1                                     
160901     MOVE '  GE' TO GOOD-STATUSCODES                                      
161001     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
161101     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
161201     PERFORM IMS-STATUSCHECK                                              
161301     IF SEGMENT-MISSING                                                   
161401         MOVE SPACE TO DCS-KDDC                                           
161501     END-IF                                                               
161601     .                                                                    
161700 IMS-STATUSCHECK SECTION.                                                 
161800                                                                          
161900     SET STATUS-IX TO 1                                                   
162000     SEARCH GOOD-STATUS                                                   
162100       AT END                                                             
162200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
162300         DELIMITED BY SIZE INTO ERROR-TEXT                                
162400         CALL FELLOG                                                      
162500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
162600         CONTINUE                                                         
162700     END-SEARCH                                                           
162800     .                                                                    
