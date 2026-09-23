000101                                                                          
000201 ID DIVISION.                                                             
000301     SKIP2                                                                
000401 PROGRAM-ID.     W9105400.                                                
000501*AUTHOR.         HENRIK ARONSSON.                                         
000601*DATE-WRITTEN.   SEPT 1992.                                               
000701                                                                          
000801*    REMARKS.                                                             
000901*                                                                         
001001*    FUNKTION:                                                            
001101*        SKAPAR FIL MED ARTIKELINFO TILL VR.                              
001201*                                                                         
001301*                                                                         
001401*    ABENDKODER:                                                          
001501*        U0016 -  . . . .                                                 
001601*        U1000 -  . . . .                                                 
001701*                                                                         
001801                                                                          
001901     SKIP3                                                                
002001 ENVIRONMENT DIVISION.                                                    
002101     SKIP2                                                                
002201 INPUT-OUTPUT SECTION.                                                    
002301                                                                          
002401 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002601*          --- FIL MED ARTIKELINFO                                        
002701     SELECT W91042                     ASSIGN TO W91054D1.                
002801     SKIP2                                                                
002901*          --- FIL MED ARTIKELINFO TILL VR                                
003001     SELECT W91052                     ASSIGN TO W91054D2.                
003101     EJECT                                                                
003201*          --- FIL MED ARTIKELINFO TILL VR                                
003301     SELECT W91043                     ASSIGN TO W91054D3.                
003404     EJECT                                                                
003804 DATA DIVISION.                                                           
003904     SKIP3                                                                
004004 FILE SECTION.                                                            
004104     SKIP3                                                                
004204 FD  W91042                                                               
004304     RECORDING       F                                                    
004404     BLOCK CONTAINS  0.                                                   
004504     SKIP2                                                                
004604*01  -COPY W91042      -L.                                                
004704     SKIP3                                                                
004804 FD  W91052                                                               
004904     RECORDING       F                                                    
005004     BLOCK CONTAINS  0.                                                   
005104     SKIP2                                                                
005204*01  POST -COPY W91054 -PRE  UT-  -L.                                     
005304     SKIP3                                                                
005404 FD  W91043                                                               
005504     RECORDING       F                                                    
005604     BLOCK CONTAINS  0.                                                   
005704     SKIP2                                                                
005804*01  POST -COPY W91058 -PRE  UTW91043-  -L.                               
006504     EJECT                                                                
006604 WORKING-STORAGE SECTION.                                                 
006704     SKIP2                                                                
006804*    -COPY WY2000W2                                                       
006904     SKIP3                                                                
007004 77  IDPGM                       PIC X(8)    VALUE 'W9105400'.            
007104 77  JA                          PIC X       VALUE 'J'.                   
007204 77  NEJ                         PIC X       VALUE 'N'.                   
007304 77  INDX                        PIC 9(2)    VALUE ZERO.                  
007404                                                                          
007504 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
007604     88  END-OF-W91042                       VALUE 'J'.                   
007704     EJECT                                                                
007804 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007904 01  FILLER REDEFINES DAGENS-DATUM.                                       
008004     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008104     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008204     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008304                                                                          
008404 01  DAGENS-TIAAVVD              PIC 9(5).                                
008504 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
008604     03  DAGENS-TIAAVV           PIC 9(4).                                
008704     03  DAGENS-TID              PIC 9(1).                                
008804                                                                          
008904*01  -COPY WWPRODSL                                                       
009004     EJECT                                                                
009104 01  DYNAMISKA-SUBPROGRAM.                                                
009204*                                                                         
009304     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009404     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009504     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009604     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
009704     SKIP2                                                                
009804*    --- PARAMETRAR TILL ABEND                                            
009904                                                                          
010004 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010104 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010204     SKIP2                                                                
010304 01  FELTEXT.                                                             
010404     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010504     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010604     EJECT                                                                
010704*    --- PARAMETRAR TILL POSTSUM                                          
010804*                                                                         
010904*01  -COPY W0005   -PRE  POSTSUM-                                         
011004 01  FILLER                     PIC X(24) VALUE 'WDATAREA '.              
011104*01  -COPY  WDATAREA                                                      
011204 01  W009VADD-AREA.                                                       
011304     03 VADD-DATUM-AAVV         PIC S9(5)  COMP-3.                        
011404     03 VADD-ANTAL              PIC S9(3)  COMP-3.                        
011504     EJECT                                                                
011604 01  IN-AREA-START               PIC X(24)   VALUE                        
011704                                 'IN-AREA-START  '.                       
011804     SKIP2                                                                
011904 01  IN-AREA.                                                             
012004     03  FILLER                  PIC X(900).                              
012104*01  FILLER -COPY W91042      -PRE IN-   -RED  IN-AREA                    
012204     EJECT                                                                
012304 01  UT-AREA-START               PIC X(24)   VALUE                        
012404                                 'UT-AREA-START  '.                       
012504     SKIP2                                                                
012604*01  AREA -COPY W91054     -PRE UT-                                       
012704     SKIP2                                                                
012804*                                                                         
012904 01  UT-AREA-W91043              PIC X(24)   VALUE                        
013004                                 'UT-AREA-W91043 '.                       
013104     SKIP2                                                                
013204*01  AREA -COPY W91058     -PRE UTW91043-                                 
013904                                                                          
014004                                                                          
014104     EJECT                                                                
014204 PROCEDURE DIVISION.                                                      
014304     SKIP2                                                                
014404                                                                          
014504     PERFORM A-INIT                                                       
014604     PERFORM S01-LAES-W91042                                              
014704     PERFORM UNTIL END-OF-W91042                                          
014804       PERFORM B-UTFIL                                                    
014904       PERFORM S01-LAES-W91042                                            
015004     END-PERFORM                                                          
015104                                                                          
015204     PERFORM Z-FINIT                                                      
015304                                                                          
015404     MOVE ZERO TO RETURN-CODE                                             
015504     GOBACK                                                               
015604     .                                                                    
015704     EJECT                                                                
015804 A-INIT SECTION.                                                          
015904                                                                          
016004     OPEN INPUT  W91042                                                   
016104                                                                          
016204     OPEN OUTPUT W91052                                                   
016304     OPEN OUTPUT W91043                                                   
016504                                                                          
016604     ACCEPT DAGENS-DATUM FROM DATE                                        
016704                                                                          
016804     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
016904     CALL WDATKONV USING DAT-KDDATFORM                                    
017004                         DAT-I-TIDATUM                                    
017104                         DAT-O-TIDATUM                                    
017204                         DAT-KDSVAR                                       
017304                                                                          
017404     MOVE DAT-TIAAVVD TO DAGENS-TIAAVVD                                   
017504                                                                          
017604     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017704     .                                                                    
017804     EJECT                                                                
017904                                                                          
018004     SKIP3                                                                
018104 B-UTFIL SECTION.                                                         
018204                                                                          
018304     MOVE IN-KDPRODSL      TO TEST-KDPRODSL                               
018404     MOVE IN-TIFINLV       TO TMP1-YYWWD                                  
018504     MOVE DAGENS-TIAAVVD   TO TMP2-YYWWD                                  
018604     PERFORM WY2000P2                                                     
018704     IF TMP1-YYWWD > TMP2-YYWWD                                           
018705     OR (IN-IDARTNR = 0 OR 41 OR 41)                                      
018904     OR KDPRODSL-VOLVO-EMB                                                
019004     OR IN-KDERS          = 52                                            
019104*    OR IN-KDUART         = 'M'                                           
019204*    OR IN-KDUART         = 'S'                                           
019304     OR IN-FLLSRDEL       = 'N'                                           
019404        CONTINUE                                                          
019504     ELSE                                                                 
019604        PERFORM BA-FLYTTA-TILL-UTFIL                                      
019704        PERFORM S11-SKRIV-W91052                                          
020004                                                                          
020104        IF IN-KDUART      = 'M'                                           
020204        OR IN-KDUART      = 'S'                                           
020304           CONTINUE                                                       
020404        ELSE                                                              
020504           PERFORM BB-FLYTTA-NL-TILL-UTFIL-W91043                         
020604           PERFORM S12-SKRIV-W91043                                       
020704                                                                          
020804           PERFORM BC-FLYTTA-ITAL-T-UTFIL-W91043                          
020904           PERFORM S12-SKRIV-W91043                                       
021004                                                                          
021104           PERFORM BD-FLYTTA-SF-TILL-UTFIL-W91043                         
021204           PERFORM S12-SKRIV-W91043                                       
021304        END-IF                                                            
021404     END-IF                                                               
021504     .                                                                    
021604     EJECT                                                                
021704                                                                          
021804 BA-FLYTTA-TILL-UTFIL SECTION.                                            
021904                                                                          
022004     MOVE IN-IDARTNR      TO UT-IDARTNR                                   
022104                                                                          
022204     MOVE SPACE           TO UT-BEART-SVE                                 
022304                             UT-BEART-ENG                                 
022404                             UT-BEART-FRA                                 
022504                             UT-BEART-SPA                                 
022604                             UT-BEART-TYS                                 
022704                             UT-IDLEVNR-DDGS                              
022804     MOVE 1 TO INDX                                                       
022904     PERFORM UNTIL INDX > 10                                              
023004       IF IN-IDSKYLT (INDX) = 'S  '                                       
023104         MOVE IN-BEART (INDX) TO UT-BEART-SVE                             
023204       END-IF                                                             
023304       IF IN-IDSKYLT (INDX) = 'GB '                                       
023404         MOVE IN-BEART (INDX) TO UT-BEART-ENG                             
023504       END-IF                                                             
023604       IF IN-IDSKYLT (INDX) = 'D  '                                       
023704         MOVE IN-BEART (INDX) TO UT-BEART-TYS                             
023804       END-IF                                                             
023904       IF IN-IDSKYLT (INDX) = 'E  '                                       
024004         MOVE IN-BEART (INDX) TO UT-BEART-SPA                             
024104       END-IF                                                             
024204       IF IN-IDSKYLT (INDX) = 'F  '                                       
024304         MOVE IN-BEART (INDX) TO UT-BEART-FRA                             
024404       END-IF                                                             
024504       ADD 1 TO INDX                                                      
024604     END-PERFORM                                                          
024704                                                                          
024804     MOVE 1 TO INDX                                                       
024904     PERFORM UNTIL INDX > 6                                               
025004        MOVE IN-IDSTATNR(INDX)  TO UT-IDSTATNR(INDX)                      
025104        ADD 1 TO INDX                                                     
025204     END-PERFORM                                                          
025304                                                                          
025404     MOVE IN-FLTPO1       TO UT-FLTPO1                                    
025504     MOVE IN-IDFKNGRP     TO UT-IDFKNGRP                                  
025604     MOVE IN-KDAGE        TO UT-KDAGE                                     
025704     MOVE IN-KDARTURS     TO UT-KDARTURS                                  
025804     MOVE IN-KDFARLIG     TO UT-KDFARLIG                                  
025904     MOVE IN-KDLTK        TO UT-KDLTK                                     
026004     MOVE IN-KDPRTILL     TO UT-KDPRTILL                                  
026104     MOVE IN-KDSORT       TO UT-KDSORT                                    
026204     MOVE IN-KDSRA        TO UT-KDSRA                                     
026304     MOVE IN-KDVSOP       TO UT-KDVSOP                                    
026404     MOVE IN-KDVVKL       TO UT-KDVVKL                                    
026504     MOVE IN-KVFRYSTI     TO UT-KVFRYSTI                                  
026604     MOVE IN-KVQPACK-0    TO UT-KVQPACK-0                                 
026704     MOVE IN-KVQPACK-1    TO UT-KVQPACK-1                                 
026804     MOVE IN-KVQPACK-2    TO UT-KVQPACK-2                                 
026904     MOVE IN-KVQPACK-3    TO UT-KVQPACK-3                                 
027004     MOVE IN-KVQPACK-4    TO UT-KVQPACK-4                                 
027104     MOVE IN-PRARTBTO-EXP TO UT-PRARTBTO-EXP                              
027204     MOVE IN-VKART        TO UT-VKART                                     
027304     MOVE IN-VLARTNTO     TO UT-VLARTNTO                                  
027404     MOVE IN-TIFINLV      TO UT-TIFINLV                                   
027405     MOVE IN-TIURPROD     TO UT-TIURPROD                                  
027504     MOVE IN-ADLAGOMR     TO UT-ADLAGOMR                                  
027505     MOVE IN-KDPRODSL     TO UT-KDPRODSL                                  
027604                                                                          
027704**   IF IN-KDPRODSL = 71                                                  
027804**     MOVE 93            TO UT-KDPRODSL                                  
027904**   ELSE                                                                 
028004**     IF IN-KDPRODSL = 72                                                
028104**       MOVE 95          TO UT-KDPRODSL                                  
028204**     ELSE                                                               
028304**       IF IN-KDPRODSL = 73                                              
028404**         MOVE 91        TO UT-KDPRODSL                                  
028504**       ELSE                                                             
028604**         IF IN-KDPRODSL = 74                                            
028704**           MOVE 94      TO UT-KDPRODSL                                  
028804**         ELSE                                                           
029004**         END-IF                                                         
029104**       END-IF                                                           
029204**     END-IF                                                             
029304**   END-IF                                                               
029404                                                                          
029504     IF IN-KDERS-UTG = 0                                                  
029604        MOVE 0       TO UT-FLABORT-UTG                                    
029704     ELSE                                                                 
029804        MOVE 1       TO UT-FLABORT-UTG                                    
029904     END-IF                                                               
030004                                                                          
030005     IF IN-KDERS    > 20                                                  
030204        MOVE IN-KDERS     TO UT-KDERS                                     
030304     ELSE                                                                 
030404        MOVE ZERO         TO UT-KDERS                                     
030504     END-IF                                                               
030604     MOVE IN-KDUART       TO UT-KDUART                                    
030704     .                                                                    
030804     EJECT                                                                
030904 BB-FLYTTA-NL-TILL-UTFIL-W91043             SECTION.                      
031004                                                                          
031104     MOVE IN-IDARTNR      TO UTW91043-IDARTNR                             
031204                                                                          
031304     MOVE SPACE           TO UTW91043-BEART                               
031404                                                                          
031504     MOVE 1 TO INDX                                                       
031604     PERFORM UNTIL INDX > 10                                              
031704       IF IN-IDSKYLT (INDX) = 'NL '                                       
031804         MOVE IN-BEART (INDX) TO UTW91043-BEART                           
031904       END-IF                                                             
032004       ADD 1 TO INDX                                                      
032104     END-PERFORM                                                          
032204                                                                          
032304     MOVE 'NL'              TO UTW91043-IDSKYLT                           
032404     .                                                                    
032504     EJECT                                                                
032604 BC-FLYTTA-ITAL-T-UTFIL-W91043             SECTION.                       
032704                                                                          
032804     MOVE IN-IDARTNR      TO UTW91043-IDARTNR                             
032904                                                                          
033004     MOVE SPACE           TO UTW91043-BEART                               
033104                                                                          
033204     MOVE 1 TO INDX                                                       
033304     PERFORM UNTIL INDX > 10                                              
033404       IF IN-IDSKYLT (INDX) = 'I  '                                       
033504         MOVE IN-BEART (INDX) TO UTW91043-BEART                           
033604       END-IF                                                             
034001       ADD 1 TO INDX                                                      
035001     END-PERFORM                                                          
036001                                                                          
036101     MOVE 'I  '             TO UTW91043-IDSKYLT                           
036201     .                                                                    
036301     EJECT                                                                
036401 BD-FLYTTA-SF-TILL-UTFIL-W91043             SECTION.                      
036501                                                                          
036601     MOVE IN-IDARTNR      TO UTW91043-IDARTNR                             
036701                                                                          
036801     MOVE SPACE           TO UTW91043-BEART                               
036901                                                                          
037001     MOVE 1 TO INDX                                                       
037101     PERFORM UNTIL INDX > 10                                              
037201       IF IN-IDSKYLT (INDX) = 'SF '                                       
037301         MOVE IN-BEART (INDX) TO UTW91043-BEART                           
037401       END-IF                                                             
037501       ADD 1 TO INDX                                                      
037601     END-PERFORM                                                          
037701                                                                          
037801     MOVE 'SF'              TO UTW91043-IDSKYLT                           
037901     .                                                                    
038004     EJECT                                                                
047301 Z-FINIT SECTION.                                                         
047401     CLOSE W91042                                                         
047501           W91052                                                         
047601           W91043                                                         
047804     SKIP2                                                                
047904     MOVE 'S' TO POSTSUM-OPKOD                                            
048004     CALL POSTSUM USING POSTSUM-PARM                                      
048104     .                                                                    
048204     EJECT                                                                
048304 S01-LAES-W91042  SECTION.                                                
048404     SKIP2                                                                
048504     READ W91042 INTO IN-AREA                                             
048604     AT END                                                               
048704        SET END-OF-W91042 TO TRUE                                         
048804                                                                          
048904     NOT AT END                                                           
049004        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
049104        MOVE 'W91042'   TO POSTSUM-FDNAMN                                 
049204        MOVE 'W91054D1' TO POSTSUM-DDNAMN2                                
049304        CALL POSTSUM USING POSTSUM-PARM                                   
049404     END-READ                                                             
049504     .                                                                    
049604     EJECT                                                                
049704 S11-SKRIV-W91052 SECTION.                                                
049804     SKIP2                                                                
049904     WRITE UT-POST FROM UT-AREA                                           
050004                                                                          
050104     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
050204     MOVE 'W91052'   TO POSTSUM-FDNAMN                                    
050304     MOVE 'W91054D2' TO POSTSUM-DDNAMN2                                   
050404     CALL POSTSUM USING POSTSUM-PARM                                      
050504     .                                                                    
050604     EJECT                                                                
050704 S12-SKRIV-W91043 SECTION.                                                
050804     SKIP2                                                                
050904     WRITE UTW91043-POST FROM UTW91043-AREA                               
051004                                                                          
051104     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
051204     MOVE 'W91043'   TO POSTSUM-FDNAMN                                    
051304     MOVE 'W91054D3' TO POSTSUM-DDNAMN2                                   
051401     CALL POSTSUM USING POSTSUM-PARM                                      
051501     .                                                                    
051604     EJECT                                                                
060001*    -COPY WY2000P2                                                       
