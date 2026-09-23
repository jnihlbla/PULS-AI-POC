000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W0050700.                                                
000301 AUTHOR.         RICHARD.                                                 
000401 DATE-WRITTEN.   APRIL 93.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*     FUNKTION.  STARTS TRANSACTIONS AND SOP-ROUTINES                     
000801*                ACCORDING GIVEN SCHEDUALES                               
000901*                                                                         
001001*     TRIGGERED BY STARTED TASK IN IMS/IMF VIA:                           
001101*     F1IMG1.IMS.TCF(TCFIMG1)                                             
001201                                                                          
001301                                                                          
001401     SKIP2                                                                
001501 ENVIRONMENT DIVISION.                                                    
001601     SKIP3                                                                
001701 DATA DIVISION.                                                           
001801     EJECT                                                                
001901 WORKING-STORAGE SECTION.                                                 
002001*    -- CHECKED BY WY2000                                                 
002101 77    IDPGM             PIC X(8)    VALUE 'W0050700'.                    
002201 77    W-COMPILED        PIC X(16)   VALUE SPACE.                         
002301 77    W-TIME-SS         PIC 9(2)    VALUE ZERO.                          
002401 77    FELTEXT           PIC X(80)   VALUE SPACE.                         
002501 77    JA                PIC X       VALUE 'J'.                           
002601 77    NEJ               PIC X       VALUE 'N'.                           
002701     SKIP3                                                                
002801 01    W-RENSA-FAELT.                                                     
002901   03    FILLER          PIC 9(3)    VALUE 053     COMP-3.                
003001     SKIP3                                                                
003101 01 W-DATE.                                                               
003201   03 W-DATE-YY          PIC 9(2)    VALUE ZERO.                          
003301   03 W-DATE-MM          PIC 9(2)    VALUE ZERO.                          
003401   03 W-DATE-DD          PIC 9(2)    VALUE ZERO.                          
003501     SKIP3                                                                
003601 01 W-TIME.                                                               
003701   03 W-TIME-HHMM        PIC 9(4)    VALUE ZERO.                          
003801                                                                          
003901**** BILLIT TIMES MUST BE COORDINATED WITH SHIPPING RTN TIMES:            
004008*    W476S3-TIMES X MINUTES BEFORE THE BILLIT SCHEDUALE!!                 
004101*    W476SX-TIMES 5 MINUTES AFTER THE BILLIT SCHEDUALE!!                  
004201****                                                                      
005806     88 BILLIT             VALUE 0000 0015 0025 0035 0045 0055            
005903                                 0105 0115 0125 0135 0145 0155            
006103                                 0205 0215 0225 0235 0245 0255            
006203                                 0305 0315 0325 0335 0345 0355            
006403                                 0405 0415 0425 0435 0445 0455            
006503                                 0505 0515 0525 0535 0545 0555            
006604                                 0605 0615 0625 0635 0645 0655            
006704                                 0705 0715 0725 0735 0745 0755            
006804                                 0805 0815 0825 0835 0845 0855            
006904                                 0905 0915 0925 0935 0945 0955            
007004                                 1005 1015 1025 1035 1045 1055            
007105                                 1105 1115 1125 1135 1145 1155            
007205                                 1205 1215 1225 1235 1245 1255            
007305                                 1305 1315 1325 1335 1345 1355            
007405                                 1405 1415 1425 1435 1445 1455            
007505                                 1505 1515 1525 1535 1545 1555            
007605                                 1605 1615 1625 1635 1645 1655            
007705                                 1705 1715 1725 1735 1745 1755            
007805                                 1805 1815 1825 1835 1845 1855            
007905                                 1905 1915 1925 1935 1945 1955            
008005                                 2005 2015 2025 2035 2045 2055            
008105                                 2105 2115 2125 2135 2145 2155            
008205                                 2205 2215 2225 2235 2245 2255            
008306                                 2310 2320 2330 2340 2350.                
008605                                                                          
008707     88 BILLIT-SUNDAY      VALUE 0000 0020 0030 0040 0050                 
009103                                 0555                                     
009203                                 0625 0655      0725 0755                 
009303                                 0825 0855      0925 0955                 
009403                                 1025 1055      1125 1155                 
009503                                 1225 1255      1325 1355                 
009603                                 1425 1455      1525 1555                 
009703                                 1625 1655      1725 1755                 
009803                                 1825 1855      1925 1955                 
009903                                 2025 2055      2125 2155                 
010003                                 2225 2255      2320 2350.                
010203                                                                          
010606     88 BILLIT-NEWYEARSEVE VALUE 0000 0015 0025 0035 0045 0055            
010703                                 0105 0115 0125 0135 0145 0155            
010903                                 0205 0215 0225 0235 0245 0255            
011003                                 0305 0315 0325 0335 0345 0355            
011203                                 0405 0415 0425 0435 0445 0455            
011303                                 0505 0515 0525 0535 0545 0555            
011503                                 0625 0655      0725 0755                 
011603                                 0825 0855      0925 0955                 
011703                                 1025 1055      1125 1155                 
011803                                 1225 1255      1325 1355                 
011903                                 1425 1455      1525 1555                 
012003                                 1625 1655      1725 1755                 
012103                                 1825 1855      1925 1955                 
012203                                 2025 2055      2125 2155                 
012306                                 2225 2255      2325 2350.                
012403                                                                          
012510     88 BILLIT-NEWYEARSDAY VALUE 0000 0020 0030 0040 0050.                
012610                                                                          
012708** W476S3 SCHEDUALES X MINUTES BEFORE BILLIT-TIMES                        
012808       88  W476S3                    VALUE      0050      0150            
012908                                                0250      0350            
013008                                                0450      0550            
013108                                           0620 0650 0720 0750            
013208                                           0820 0850 0920 0950            
013308                                           1020 1050 1120 1150            
013408                                           1220 1250 1320 1350            
013508                                           1420 1450 1520 1550            
013608                                           1620 1650 1720 1750            
013708                                           1820 1850 1920 1950            
013808                                           2020 2050 2120 2150            
013908                                           2220 2250 2315 2345.           
017003                                                                          
017109       88  W476S3-SUNDAY             VALUE 0015 0045                      
017309                                           0620 0720 0820 0920            
017409                                           1020 1120 1220 1320            
017509                                           1420 1520 1620 1720            
017609                                           1820 1920 2020 2120            
017709                                           2220 2320 2345.                
018205                                                                          
018609       88  W476S3-NEWYEARSEVE        VALUE      0050      0150            
018709                                                0250      0350            
018809                                                0450      0550            
018909                                           0620 0650 0720 0750            
019009                                           0820 0850 0920 0950            
019109                                           1020 1050 1120 1150            
019209                                           1220 1250 1320 1350            
019309                                           1420 1450 1520 1550            
019409                                           1620 1650 1720 1750            
019509                                           1820 1850 1920 1950            
019609                                           2020 2050 2120 2150            
019709                                           2220 2250 2320 2345.           
021003                                                                          
021109       88  W476S3-NEWYEARSDAY        VALUE 0015 0045.                     
021209                                                                          
021303* W476SX TO SCHEDUALE 5 MINUTES AFTER BILLIT-TIMES                        
021403* W476SX STARTS W476S5 IF INVOICE DATA EXISTS                             
021509       88 W476SX             VALUE      0010 0020 0030 0040 0050          
021609                                        0110 0120 0130 0140 0150          
021709                                   0200 0210 0220 0230 0240 0250          
021809                                   0300 0310 0320 0330 0340 0350          
021909                                   0400 0410 0420 0430 0440 0450          
022009                                        0510 0520 0530 0540 0550          
022109                                   0600 0610 0620 0630 0640 0650          
022209                                   0700 0710 0720 0730 0740 0750          
022309                                   0800 0810 0820 0830 0840 0850          
022409                                   0900 0910 0920 0930 0940 0950          
022509                                   1000 1010 1020 1030 1040 1050          
022609                                   1100 1110 1120 1130 1140 1150          
022709                                   1200 1210 1220 1230 1240 1250          
022809                                   1300 1310 1320 1330 1340 1350          
022909                                   1400 1410 1420 1430 1440 1450          
023009                                   1505 1510 1520 1530 1540 1550          
023109                                        1610 1620 1630 1640 1650          
023209                                   1700 1710 1720 1730 1740 1750          
023309                                   1800 1810 1820 1830 1840 1850          
023409                                   1900 1910 1920 1930 1940 1950          
023509                                        2010 2020 2030 2040 2050          
023609                                   2100 2110 2120 2130 2140 2150          
023709                                   2200 2210 2220 2230 2240 2250          
023809                                   2300 2315 2325 2335 2345 2355.         
023903                                                                          
024009       88 W476SX-SUNDAY      VALUE 0010 0025 0035 0045 0055               
024309                                   0600 0630 0700 0730 0800 0830          
024409                                   0900 0930 1000 1030 1100 1130          
024509                                   1200 1230 1300 1330 1400 1430          
024609                                   1500 1530 1600 1630 1700 1730          
024709                                   1800 1830 1900 1930 2000 2030          
024809                                   2100 2130 2200 2230 2300 2325          
024909                                   2355.                                  
026203                                                                          
026509       88 W476SX-NEWYEARSEVE VALUE      0010 0020 0030 0040 0050          
026609                                        0110 0120 0130 0140 0150          
026709                                   0200 0210 0220 0230 0240 0250          
026809                                   0300 0310 0320 0330 0340 0350          
026909                                   0400 0410 0420 0430 0440 0450          
027009                                   0500 0510 0520 0530 0540 0550          
027109                                   0600 0630      0700 0730               
027209                                   0800 0830      0900 0930               
027309                                   1000 1030      1100 1130               
027409                                   1200 1230      1300 1330               
027509                                   1400 1430      1500 1530               
027609                                   1600 1630      1700 1730               
027709                                   1800 1830      1900 1930               
027809                                   2000 2030      2100 2130               
027909                                   2200 2230      2300 2330 2355.         
029405                                                                          
029509       88 W476SX-NEWYEARSDAY VALUE 0010 0025 0035 0045 0055.              
029609                                                                          
029705       88  PRICEIT-GRP-WORKDAY       VALUE 0530 0630 0730                 
029805                                           0830 0930 1030                 
029905                                           1130 1230 1330                 
030005                                           1430 1530 1630                 
030105                                           1730 1830 1930.                
030205                                                                          
030305       88  PRICEIT-GRP-SATURDAY      VALUE 0530 0630 0730                 
030405                                           0830 0930 1030                 
030505                                           1130 1230 1330                 
030605                                           1430 1530.                     
030705                                                                          
030805       88  PRICEIT-WORKDAY           VALUE 0600 0700 0800                 
030905                                           0900 1000 1100                 
031005                                           1200 1300 1400                 
031105                                           1500 1600 1700                 
031205                                           1800 1900.                     
031305                                                                          
031405       88  PRICEIT-SATURDAY          VALUE 0600 0700 0800                 
031505                                           0900 1000 1100                 
031605                                           1200 1300 1400                 
031705                                           1500 1600.                     
031805                                                                          
031905       88  W476SE-WORKDAY            VALUE 0700 0710 0720                 
032005                                           0730 0740 0750                 
032105                                           0800 0810 0820                 
032205                                           0830 0840 0850                 
032305                                           0900 0910 0920                 
032405                                           0930 0940 0950                 
032505                                           1000 1010 1020                 
032605                                           1030 1040 1050                 
032705                                           1100 1110 1120                 
032805                                           1130 1140 1150                 
032905                                           1200 1210 1220                 
033005                                           1230 1240 1250                 
033105                                           1300 1310 1320                 
033205                                           1330 1340 1350                 
033305                                           1400 1410 1420                 
033405                                           1430 1440 1450                 
033505                                           1500 1510 1520                 
033605                                           1530 1540 1550                 
033705                                           1600 1610 1620                 
033805                                           1630 1640 1650                 
033905                                           1700 1710 1720                 
034005                                           1730 1740 1750                 
034105                                     1800  1830 1900 1930                 
034205                                     2000  2030 2100 2130                 
034305                                     2200  2230 2300.                     
034405                                                                          
034505       88  W476SE-SATURDAY           VALUE 0700 0730 0800 0830            
034605                                           0900 0930 1000 1030            
034705                                           1100 1130 1200 1230            
034805                                           1330 1400 1430 1530            
034905                                           1600 1630 1700 1800            
035005                                           1900 1930 2100 0000.           
035105                                                                          
035205       88  W463D8-WORKDAY            VALUE 0545.                          
035305                                                                          
035405       88  W612D9-WORKDAY            VALUE 0600 1450.                     
035505                                                                          
035605       88  4638-WORKDAY              VALUE 0600 0800 1000                 
035705                                           1200 1400 1600                 
035805                                           1800 2000 2200.                
035905       88  4638-SATURDAY             VALUE 0600 0800 1000                 
036005                                           1200 1400 1600.                
036105                                                                          
036205       88  4207-WORKDAY              VALUE                                
036305                            0600 0630 0700 0730 0800 0830                 
036405                            0900 0930 1000 1030 1100 1130                 
036505                            1200 1230 1300 1330 1400 1430                 
036605                            1500 1530 1600 1630 1700 1730                 
036705                            1800 1830 1900 1930 2000 2030                 
036805                            2100 2130.                                    
036905                                                                          
037005       88  4789-SATURDAY             VALUE 0900.                          
037105                                                                          
037205       88  4789-YEAREND              VALUE 2130.                          
037305                                                                          
041405       88  W412S4-WORKDAY            VALUE                                
041505                            0545 0645 0745 0845                           
041605                            0945 1045 1145 1245                           
041705                            1345 1445 1545 1645                           
041805                            1745 1845 1945 2045.                          
041905                                                                          
043005                                                                          
043006       88  W412S4-EOY                VALUE 0600 1200.                     
043020                                                                          
043030                                                                          
043508                                                                          
043605 01  FILLER REDEFINES W-TIME.                                             
043705   03  W-TIME-HH         PIC 9(2).                                        
043805   03  W-TIME-MM         PIC 9(2).                                        
043905     SKIP3                                                                
044005*      --- VALID IDDC CODES                                               
044105*                                                                         
044205       EJECT                                                              
044305 01    DYNAMISKA-SUBPROGRAM.                                              
044405   03    WDATKONV        PIC X(8)    VALUE 'WDATKONV'.                    
044505   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
044605   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
044705     EJECT                                                                
044805 01  FILLER              PIC X(16)   VALUE ' WDATAREA'.                   
044905*01    -COPY WDATAREA                                                     
045005     EJECT                                                                
045105 01    MID-W0I50701.                                                      
045205   03    MID-KDTRANS     PIC X(8).                                        
045305   03    MID-INDATA.                                                      
045405     05    MID-INDATA-1  PIC X(1).                                        
045505     05    FILLER        PIC X(49).                                       
045605     EJECT                                                                
045705*01      -COPY WMSGAREA                                                   
045805     EJECT                                                                
045905   03    MOD-W0O50701        REDEFINES MSG-AREA.                          
046005     05    MOD-IDTRANS   PIC X(4).                                        
046105     05    MOD-TEMFSFEL  PIC X(40).                                       
046205     05    MOD-KDTRANS   PIC X(8).                                        
046305     05    MOD-INDATA    PIC X(50).                                       
046405     05    MOD-TEMFSINF  PIC X(55).                                       
046505     05    FILLER        PIC X(1776).                                     
046605     EJECT                                                                
046705 01  FILLER              PIC X(16)   VALUE ' WMSGSPAR'.                   
046805*01      -COPY WMSGSPAR                                                   
046905     EJECT                                                                
047005 01  FILLER              PIC X(16)   VALUE ' WMSGSOP-AREA'.               
047105*01      -COPY WMSGSOP                                                    
047205     EJECT                                                                
047305******************************************************************        
047405*                                                                         
047505*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
047605*                                                                         
047705 01  IMS-WS.                                                              
047805   03    FILLER              PIC X(16)   VALUE ' IMS-WS     '.            
047905     SKIP3                                                                
048005   03    STATUS-WS           PIC XX.                                      
048105     88  STATUS-OK                       VALUE '  '.                      
048205     88  SEGMENT-FINNS                   VALUE '  '.                      
048305     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
048405     88  TRANSKOD-FEL                    VALUE 'A1'.                      
048505     88  SECURITY-FEL                    VALUE 'A4'.                      
048605     SKIP3                                                                
048705   03    GODK-STATUSKODER.                                                
048805     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048905     EJECT                                                                
049005*01      -COPY W0003                                                      
049105     EJECT                                                                
049205 LINKAGE SECTION.                                                         
049305*01  -COPY W0009     -PRE MSG-                                            
049405     EJECT                                                                
049505*01  -COPY W0009     -PRE ALT-                                            
049605     SKIP2                                                                
049705*01  -COPY W0009     -PRE ALTF204-                                        
049805     EJECT                                                                
049905*01  -COPY W0009     -PRE ALT4638-                                        
050005     EJECT                                                                
050105*01  -COPY W0009     -PRE ALT0606-                                        
050205     EJECT                                                                
050305*01  -COPY W0009     -PRE ALT4789-                                        
050405     EJECT                                                                
050705 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ALTF204-PCB                     
050805                              ALT4638-PCB ALT0606-PCB                     
050905                              ALT4789-PCB.                                
051005 MAIN SECTION.                                                            
051105     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALTF204-PCB                    
051205                              ALT4638-PCB ALT0606-PCB                     
051305                              ALT4789-PCB.                                
051405     PERFORM IMS-GET-MSG                                                  
051505     IF SEGMENT-FINNS                                                     
051605       PERFORM A-INIT                                                     
051705       PERFORM B-SPARA-INPUT                                              
051805       IF MSG-SPAR-UPPD-X                                                 
051905         IF MSG-SPAR-IDTRANS = 'IMF ' OR MID-KDTRANS = SPACE              
052005           PERFORM D-BILLIT-START                                         
052105           PERFORM E-GET-PRICE-START                                      
052205           PERFORM F-SHIPINGS-STARTS                                      
052305           PERFORM G-4638-START                                           
052405           PERFORM I-W463D8-START                                         
052505           PERFORM J-4789-START                                           
052705           PERFORM L-W011S3-START                                         
052805           PERFORM M-W612D9-START                                         
052905           PERFORM N-WF10M2-START                                         
053005           PERFORM O-W412XX-START                                         
053006           PERFORM P-WF10R3-START                                         
053105           IF W-TIME-MM = 00 OR 30                                        
053205             PERFORM H-START-SOPTIME                                      
053305           END-IF                                                         
053405         ELSE                                                             
053505           PERFORM X-ANNAN-START                                          
053605         END-IF                                                           
053705       ELSE                                                               
053805         MOVE 'PRESS PF11 TO START   ' TO MOD-TEMFSFEL                    
053905         MOVE MID-KDTRANS TO MOD-KDTRANS                                  
054005         MOVE MID-INDATA  TO MOD-INDATA                                   
054105         PERFORM IMS-INSERT-MSG                                           
054205       END-IF                                                             
054305     END-IF                                                               
054405     MOVE ZERO TO RETURN-CODE                                             
054505     GOBACK                                                               
054605     .                                                                    
054705     EJECT                                                                
054805 A-INIT SECTION.                                                          
054905                                                                          
055005     MOVE WHEN-COMPILED TO W-COMPILED                                     
055105                                                                          
055205     IF MSG-DUBBLA-TRANSKODER                                             
055305       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I50701                 
055405       MOVE MSG-IDTRANS-2 TO MSG-SPAR-IDTRANS                             
055505       MOVE MSG-KDMFSFOR-2 TO MSG-SPAR-KDMFSFOR                           
055605     ELSE                                                                 
055705       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W0I50701                   
055805       MOVE MSG-IDTRANS-1 TO MSG-SPAR-IDTRANS                             
055905       MOVE MSG-KDMFSFOR-1 TO MSG-SPAR-KDMFSFOR                           
056005     END-IF                                                               
056105                                                                          
056205     MOVE MSG-KDTRTYP TO MSG-SPAR-KDTRTYP                                 
056305     MOVE MSG-IDPFK   TO MSG-SPAR-IDPFK                                   
056405                                                                          
056505     MOVE LOW-VALUE   TO MSG-AREA                                         
056605     MOVE 'W0O50701'  TO MSG-SPAR-MODNAMN                                 
056705     MOVE '0507'      TO MOD-IDTRANS                                      
056805                         MSGSOP-IDTRANS                                   
056905     MOVE 'O'         TO MSGSOP-KDSOPFUNK                                 
057005                                                                          
057105     MOVE W-RENSA-FAELT TO MOD-TEMFSFEL                                   
057205                           MOD-KDTRANS                                    
057305                           MOD-INDATA                                     
057405                           MOD-TEMFSINF                                   
057505                                                                          
057605     ACCEPT W-TIME FROM TIME                                              
057705     ACCEPT W-DATE FROM DATE                                              
057805                                                                          
057905     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
058005     CALL WDATKONV USING DAT-KDDATFORM                                    
058105                         DAT-I-TIDATUM                                    
058205                         DAT-O-TIDATUM                                    
058305                         DAT-KDSVAR                                       
058405     .                                                                    
058505     EJECT                                                                
058605 B-SPARA-INPUT SECTION.                                                   
058705                                                                          
058805     IF MID-KDTRANS = '++++++++'                                          
058905       MOVE W-RENSA-FAELT TO MID-KDTRANS                                  
059005     END-IF                                                               
059105     IF MID-INDATA-1 = '+'                                                
059205       MOVE W-RENSA-FAELT TO MID-INDATA                                   
059305     END-IF                                                               
059405     .                                                                    
059505     EJECT                                                                
059605                                                                          
059705 D-BILLIT-START SECTION.                                                  
059811                                                                          
059905     IF  DAT-TIMM = 12                                                    
060005     AND DAT-TIDD = 31                                                    
060105     AND BILLIT-NEWYEARSEVE                                               
060205       ADD +17 TO MSG-KVLL                                                
060305       MOVE 'WF0204X ' TO MSG-KDTRANS-1                                   
060405       MOVE '0507' TO MSG-IDTRANS-1                                       
060505       MOVE '1' TO MSG-KDMFSFOR-1                                         
060605       MOVE SPACE TO MSG-MID-OUT                                          
060705       PERFORM IMS-INSERT-ALTMSG-WF0204                                   
060805     ELSE                                                                 
060905       IF  DAT-TIMM = 01                                                  
061005       AND DAT-TIDD = 01                                                  
061105       AND BILLIT-NEWYEARSDAY                                             
061205         ADD +17 TO MSG-KVLL                                              
061305         MOVE 'WF0204X ' TO MSG-KDTRANS-1                                 
061405         MOVE '0507' TO MSG-IDTRANS-1                                     
061505         MOVE '1' TO MSG-KDMFSFOR-1                                       
061605         MOVE SPACE TO MSG-MID-OUT                                        
061705         PERFORM IMS-INSERT-ALTMSG-WF0204                                 
061805       ELSE                                                               
061905         IF (DAT-TID < 7 AND BILLIT)                                      
062005         OR (DAT-TID = 7 AND BILLIT-SUNDAY)                               
062105           ADD +17 TO MSG-KVLL                                            
062205           MOVE 'WF0204X ' TO MSG-KDTRANS-1                               
062305           MOVE '0507' TO MSG-IDTRANS-1                                   
062405           MOVE '1' TO MSG-KDMFSFOR-1                                     
062505           MOVE SPACE TO MSG-MID-OUT                                      
062605           PERFORM IMS-INSERT-ALTMSG-WF0204                               
062705         END-IF                                                           
062805       END-IF                                                             
062905     END-IF                                                               
063005     .                                                                    
063105     EJECT                                                                
063205                                                                          
063305 E-GET-PRICE-START SECTION.                                               
063405                                                                          
063505     IF ((DAT-TID < 6) AND PRICEIT-WORKDAY)  OR                           
063605        ((DAT-TID = 6) AND PRICEIT-SATURDAY)                              
063705                                                                          
063805       MOVE 'W335S6' TO MSGSOP-IDPROCESS                                  
063905                                                                          
064005       PERFORM IMS-PURGE-ALTMSG-0606                                      
064105     END-IF                                                               
064205                                                                          
064305     IF ((DAT-TID < 6) AND PRICEIT-GRP-WORKDAY) OR                        
064405        ((DAT-TID = 6) AND PRICEIT-GRP-SATURDAY)                          
064505                                                                          
064605       MOVE 'W335S7' TO MSGSOP-IDPROCESS                                  
064705                                                                          
064805       PERFORM IMS-PURGE-ALTMSG-0606                                      
064905     END-IF                                                               
065005     .                                                                    
065105     EJECT                                                                
065205 F-SHIPINGS-STARTS SECTION.                                               
065305                                                                          
065405     IF  DAT-TIMM = 12 AND DAT-TIDD = 31                                  
065505       IF  W476SE-SATURDAY                                                
065605         MOVE 'W476SE' TO MSGSOP-IDPROCESS                                
065705         PERFORM IMS-PURGE-ALTMSG-0606                                    
065805       END-IF                                                             
065905       IF  W476S3-NEWYEARSEVE                                             
066005         MOVE 'W476S3' TO MSGSOP-IDPROCESS                                
066105         PERFORM IMS-PURGE-ALTMSG-0606                                    
066205       END-IF                                                             
066305       IF  W476SX-NEWYEARSEVE                                             
066405         MOVE 'W476SX' TO MSGSOP-IDPROCESS                                
066505         PERFORM IMS-PURGE-ALTMSG-0606                                    
066605       END-IF                                                             
066705     ELSE                                                                 
066805       IF  DAT-TIMM = 01 AND DAT-TIDD = 01                                
066905         IF  W476SE-SATURDAY                                              
067005           MOVE 'W476SE' TO MSGSOP-IDPROCESS                              
067105           PERFORM IMS-PURGE-ALTMSG-0606                                  
067205         END-IF                                                           
067305         IF  W476S3-NEWYEARSDAY                                           
067405           MOVE 'W476S3' TO MSGSOP-IDPROCESS                              
067505           PERFORM IMS-PURGE-ALTMSG-0606                                  
067605         END-IF                                                           
067705         IF  W476SX-NEWYEARSDAY                                           
067805           MOVE 'W476SX' TO MSGSOP-IDPROCESS                              
067905           PERFORM IMS-PURGE-ALTMSG-0606                                  
068005         END-IF                                                           
068105       ELSE                                                               
068205         IF (DAT-TID < 6 AND W476SE-WORKDAY) OR                           
068305            (DAT-TID = 6 AND W476SE-SATURDAY)                             
068405           MOVE 'W476SE' TO MSGSOP-IDPROCESS                              
068505           PERFORM IMS-PURGE-ALTMSG-0606                                  
068605         END-IF                                                           
068705         IF DAT-TID < 7                                                   
068805           IF W476S3                                                      
068905             MOVE 'W476S3' TO MSGSOP-IDPROCESS                            
069005             PERFORM IMS-PURGE-ALTMSG-0606                                
069105           END-IF                                                         
069205           IF W476SX                                                      
069301             MOVE 'W476SX' TO MSGSOP-IDPROCESS                            
070001             PERFORM IMS-PURGE-ALTMSG-0606                                
080001           END-IF                                                         
090001         END-IF                                                           
100001         IF DAT-TID = 7                                                   
110001           IF W476S3-SUNDAY                                               
120001             MOVE 'W476S3' TO MSGSOP-IDPROCESS                            
130001             PERFORM IMS-PURGE-ALTMSG-0606                                
131001           END-IF                                                         
131101           IF W476SX-SUNDAY                                               
131201             MOVE 'W476SX' TO MSGSOP-IDPROCESS                            
131301             PERFORM IMS-PURGE-ALTMSG-0606                                
131401           END-IF                                                         
131501         END-IF                                                           
131601       END-IF                                                             
131701     END-IF                                                               
131801     .                                                                    
131901     EJECT                                                                
132001 G-4638-START SECTION.                                                    
133001                                                                          
134001     IF ((DAT-TID < 6) AND 4638-WORKDAY)   OR                             
134101        ((DAT-TID = 6) AND 4638-SATURDAY)                                 
134201                                                                          
134301       ADD +17  TO MSG-KVLL                                               
134401       MOVE 'W40638X ' TO MSG-KDTRANS-1                                   
134501       MOVE '0507' TO MSG-IDTRANS-1                                       
134601       MOVE '1' TO MSG-KDMFSFOR-1                                         
134701       MOVE SPACE TO MSG-MID-OUT                                          
134801       PERFORM IMS-INSERT-ALTMSG-W40638                                   
134901     END-IF                                                               
135001     .                                                                    
135101     SKIP3                                                                
135201 H-START-SOPTIME SECTION.                                                 
135301                                                                          
135401     STRING 'DATETIME('                                                   
135501            W-DATE-DD '/' W-DATE-MM '/' W-DATE-YY ' '                     
135601            W-TIME-HH ':' W-TIME-MM ':' W-TIME-SS                         
135701            ')'                                                           
135801          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
135901     MOVE 'W980JIMS' TO MSGSOP-IDPROCESS                                  
136001     PERFORM IMS-PURGE-ALTMSG-0606                                        
136101     EJECT                                                                
136201     .                                                                    
136301                                                                          
136401 I-W463D8-START    SECTION.                                               
136501     IF DAT-TID < 7                                                       
136601                                                                          
136701       IF W463D8-WORKDAY                                                  
136801         MOVE 'W463D8' TO MSGSOP-IDPROCESS                                
136901         PERFORM IMS-PURGE-ALTMSG-0606                                    
137001       END-IF                                                             
137101     END-IF                                                               
137201     .                                                                    
137301     EJECT                                                                
137401                                                                          
137501 J-4789-START SECTION.                                                    
137601                                                                          
137701     IF DAT-TID = 6                                                       
137801       IF 4789-SATURDAY                                                   
137901         ADD +17  TO MSG-KVLL                                             
138001         MOVE 'W4T789X ' TO MSG-KDTRANS-1                                 
138101         MOVE '0507' TO MSG-IDTRANS-1                                     
138201         MOVE '1' TO MSG-KDMFSFOR-1                                       
138301         MOVE SPACE TO MSG-MID-OUT                                        
138401         PERFORM IMS-INSERT-ALTMSG-W40789                                 
138501       END-IF                                                             
138601     ELSE                                                                 
138701       IF  DAT-TIMM = 12                                                  
138801       AND DAT-TIDD = 31                                                  
138901       AND 4789-YEAREND                                                   
139001         ADD +17  TO MSG-KVLL                                             
139101         MOVE 'W4T789X ' TO MSG-KDTRANS-1                                 
139201         MOVE '0507' TO MSG-IDTRANS-1                                     
139301         MOVE '1' TO MSG-KDMFSFOR-1                                       
139401         MOVE SPACE TO MSG-MID-OUT                                        
139501         PERFORM IMS-INSERT-ALTMSG-W40789                                 
139601       END-IF                                                             
139701     END-IF                                                               
139801     .                                                                    
139901     SKIP3                                                                
142201 L-W011S3-START SECTION.                                                  
142301                                                                          
142401     IF DAT-TIDD = 01 AND W-TIME-HHMM = 0030                              
142501       MOVE 'W011S3' TO MSGSOP-IDPROCESS                                  
142601       PERFORM IMS-PURGE-ALTMSG-0606                                      
142701     END-IF                                                               
142801     .                                                                    
142901                                                                          
143001     EJECT                                                                
143101     SKIP3                                                                
143201 M-W612D9-START    SECTION.                                               
143301     IF DAT-TID < 6 OR (DAT-TIMM = 12 AND DAT-TIDD = 30 OR 31)            
143401                                                                          
143501       IF W612D9-WORKDAY                                                  
143601         MOVE 'W612D9' TO MSGSOP-IDPROCESS                                
143701         PERFORM IMS-PURGE-ALTMSG-0606                                    
143801       END-IF                                                             
143901     END-IF                                                               
144001     .                                                                    
144101     EJECT                                                                
144201                                                                          
144301 N-WF10M2-START    SECTION.                                               
144401                                                                          
144501*** ORDER EVERY MONTH ON 07TH AT 9 O'CLOCK                                
144601     IF W-DATE-DD = 07 AND W-TIME-HHMM = 0900                             
144701         MOVE 'WF10M2' TO MSGSOP-IDPROCESS                                
144801         PERFORM IMS-PURGE-ALTMSG-0606                                    
144900     END-IF                                                               
145001     .                                                                    
145101     EJECT                                                                
145201 O-W412XX-START    SECTION.                                               
145301                                                                          
145401     IF DAT-TID < 6                                                       
145501       IF W412S4-WORKDAY                                                  
145601         MOVE 'W412S4' TO MSGSOP-IDPROCESS                                
145701         PERFORM IMS-PURGE-ALTMSG-0606                                    
145801       END-IF                                                             
147102     ELSE                                                                 
147103       IF DAT-TIMM = 12 AND (DAT-TIDD = 30 OR 31)                         
147104         IF W412S4-EOY                                                    
147105           MOVE 'W412S4' TO MSGSOP-IDPROCESS                              
147106           PERFORM IMS-PURGE-ALTMSG-0606                                  
147107         END-IF                                                           
147130       END-IF                                                             
147201     END-IF                                                               
147301     .                                                                    
147401     EJECT                                                                
147402 P-WF10R3-START    SECTION.                                               
147403                                                                          
147404*** ORDER EVERY MONTH ON 25TH AT 18:00                                    
147405     IF W-DATE-DD = 25 AND W-TIME-HHMM = 1800                             
147406         MOVE 'WF10R3' TO MSGSOP-IDPROCESS                                
147407         PERFORM IMS-PURGE-ALTMSG-0606                                    
147408     END-IF                                                               
147409     .                                                                    
147410     EJECT                                                                
147501                                                                          
147601 X-ANNAN-START SECTION.                                                   
147700     ADD +17  TO MSG-KVLL                                                 
147800     MOVE MID-KDTRANS TO MSG-KDTRANS-1                                    
147900     MOVE '0507' TO MSG-IDTRANS-1                                         
148000     MOVE '1' TO MSG-KDMFSFOR-1                                           
148100     PERFORM IMS-CHANGE-ALTMSG                                            
148200     IF STATUS-OK                                                         
148300       PERFORM IMS-INSERT-ALTMSG                                          
148400     ELSE                                                                 
148500       STRING 'WRONG TRANS   '                                            
148600               MID-KDTRANS ' ' STATUS-WS                                  
148700              DELIMITED BY SIZE INTO MOD-TEMFSINF                         
148800       MOVE +68 TO MSG-KVLL                                               
148900       PERFORM IMS-INSERT-MSG                                             
149000     END-IF                                                               
149100     .                                                                    
149200     EJECT                                                                
149300* IMS SEKTIONER                                                           
149400     SKIP2                                                                
149500 IMS-GET-MSG SECTION.                                                     
149600     MOVE '  QCCF' TO GODK-STATUSKODER                                    
149700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
149800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     SKIP2                                                                
150200 IMS-INSERT-MSG SECTION.                                                  
150300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
150400     MOVE SPACE TO GODK-STATUSKODER                                       
150500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MSG-SPAR-MODNAMN         
150600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000 IMS-CHANGE-ALTMSG SECTION.                                               
151100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151200     MOVE '  A1A4' TO GODK-STATUSKODER                                    
151300     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
151400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700     SKIP3                                                                
151800 IMS-INSERT-ALTMSG SECTION.                                               
151900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152000     MOVE SPACE TO GODK-STATUSKODER                                       
152100     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
152200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     SKIP3                                                                
152600 IMS-INSERT-ALTMSG-WF0204 SECTION.                                        
152700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152800     MOVE SPACE TO GODK-STATUSKODER                                       
152900     CALL CBLTDLI USING ISRT ALTF204-PCB MSG-IO-AREA                      
153000     MOVE ALTF204-STATUS-CODE TO STATUS-WS                                
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     SKIP3                                                                
153400 IMS-INSERT-ALTMSG-W40638 SECTION.                                        
153500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
153600     MOVE SPACE TO GODK-STATUSKODER                                       
153700     CALL CBLTDLI USING ISRT ALT4638-PCB MSG-IO-AREA                      
153800     MOVE ALT4638-STATUS-CODE TO STATUS-WS                                
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100     SKIP3                                                                
154200 IMS-INSERT-ALTMSG-W40789 SECTION.                                        
154300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
154400     MOVE SPACE TO GODK-STATUSKODER                                       
154500     CALL CBLTDLI USING ISRT ALT4789-PCB MSG-IO-AREA                      
154600     MOVE ALT4789-STATUS-CODE TO STATUS-WS                                
154700     PERFORM IMS-STATUSKONTROLL                                           
154800     .                                                                    
154900     SKIP3                                                                
155800 IMS-PURGE-ALTMSG-0606 SECTION.                                           
155900     MOVE SPACE TO GODK-STATUSKODER                                       
156000     CALL CBLTDLI USING PURG ALT0606-PCB MSGSOP-WMSGSOP                   
156100     MOVE ALT0606-STATUS-CODE TO STATUS-WS                                
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-STATUSKONTROLL SECTION.                                              
156600                                                                          
156700     SET STATUS-IX TO 1                                                   
156800     SEARCH GODK-STATUS                                                   
156900       AT END                                                             
157000         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT                        
157100         CALL FELLOG                                                      
157200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
157300         CONTINUE                                                         
157400     END-SEARCH                                                           
157500     .                                                                    
