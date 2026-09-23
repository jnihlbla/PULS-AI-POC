010000 ID DIVISION.                                                             
020000                                                                          
030000 PROGRAM-ID.     W2319400.                                                
040000 AUTHOR.         BODIL LINDAHL.                                           
050000 DATE-WRITTEN.   97/04/29.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNKTION:                                                            
100000*        LÄSER SRS-INFO W02009 OCH W02031                                 
110000*        SKAPAR UTFIL FÖR AKTUELLT NDC                                    
120000*                                                                         
180000                                                                          
190000     SKIP3                                                                
200000 ENVIRONMENT DIVISION.                                                    
210000     SKIP2                                                                
220000 INPUT-OUTPUT SECTION.                                                    
230000                                                                          
240000 FILE-CONTROL.                                                            
250100     SKIP2                                                                
250200*          --- SRS-INFO                                                   
250300     SELECT INFIL                      ASSIGN TO W23194D1.                
250500*          --- SRS-INFO FÖR AKTUELLT NDC                                  
251000     SELECT UTFIL                      ASSIGN TO W23194D2.                
252212*          --- GODKANDA-DISTRIKT                                          
252312     SELECT DISTRKOLL                  ASSIGN TO W23194D4.                
270000     EJECT                                                                
280000 DATA DIVISION.                                                           
290000     SKIP3                                                                
300000 FILE SECTION.                                                            
310100     SKIP3                                                                
310200 FD  INFIL                                                                
310300     RECORDING       F                                                    
310400     BLOCK CONTAINS  0.                                                   
310604*01  -COPY W02009       -PRE IN   -L.                                     
310700     SKIP3                                                                
310800 FD  UTFIL                                                                
310900     RECORDING       F                                                    
311000     BLOCK CONTAINS  0.                                                   
312008*01  POST -COPY W23194 -PRE  UT-  -L.                                     
320000     EJECT                                                                
327012 FD  DISTRKOLL                                                            
328012     RECORDING       F                                                    
329012     BLOCK CONTAINS  0.                                                   
329112     SKIP2                                                                
329212 01  POST                        PIC X(80).                               
329318     EJECT                                                                
330000 WORKING-STORAGE SECTION.                                                 
340000                                                                          
340113                                                                          
341013*    -- CHECKED BY WY2000                                                 
350000 77  IDPGM                       PIC X(8)    VALUE 'W2319400'.            
360014 77  TAB-IX                      PIC S9(5)   VALUE ZERO COMP-3.           
360116 77  TAB-IX-MAX                  PIC S9(5)   VALUE 150  COMP-3.           
360218 77  WS-DISTR-SVAR               PIC 9(3)    VALUE ZERO.                  
361013 77  JA                          PIC X       VALUE 'J'.                   
370000 77  NEJ                         PIC X       VALUE 'N'.                   
380004 77  WS-IDARTNR-SPAR             PIC 9(9)    VALUE ZERO.                  
390100                                                                          
390204 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
391004     88  END-OF-INFIL                        VALUE 'J'.                   
400004                                                                          
401012 77  INFIL-DISTR-SW              PIC X       VALUE 'N'.                   
402012     88  END-OF-DISTRKOLL                    VALUE 'J'.                   
403012                                                                          
410000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
420000 01  FILLER REDEFINES DAGENS-DATUM.                                       
430000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
440000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
450000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
460000     EJECT                                                                
460100*01  -COPY WWDC99                                                         
460200     EJECT                                                                
470000 01  DYNAMISKA-SUBPROGRAM.                                                
480000*                                                                         
501000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
510000     SKIP2                                                                
610200*    --- PARAMETRAR TILL POSTSUM                                          
610300*                                                                         
611000*01  -COPY W0005   -PRE  POSTSUM-                                         
630100     EJECT                                                                
630204 01  NDC-AREA-START              PIC X(24)   VALUE                        
630304                                 'NDC-AREA-START '.                       
630504 01  NDC-POST.                                                            
630604     03  FILLER             PIC X(2).                                     
630704     03  AKTUELLT-NDC       PIC X(2).                                     
630804     03  FILLER             PIC X(76).                                    
630913                                                                          
631013 01  DISTRKOLL-TABELL.                                                    
631113     03  DISTR OCCURS 150.                                                
631213         05  TAB-FOM             PIC 9(4).                                
631319         05  FILLER              PIC X.                                   
631419         05  TAB-TOM             PIC 9(4).                                
631519         05  FILLER              PIC X.                                   
631619         05  TAB-SVAR            PIC 9(3).                                
631719 01  IN-AREA-START               PIC X(24)   VALUE                        
631819                                 'IN-AREA-START '.                        
631919     EJECT                                                                
632005*01  AREA -COPY W02009      -PRE IN-                                      
632104     EJECT                                                                
632204 01  UT-AREA-START               PIC X(24)   VALUE                        
632304                                 'UT-AREA-START  '.                       
632404                                                                          
633008*01  AREA -COPY W23194     -PRE UT-                                       
640000     EJECT                                                                
650000 PROCEDURE DIVISION.                                                      
660000 MAIN SECTION.                                                            
690000                                                                          
700000     PERFORM A-INIT                                                       
711004     PERFORM S01-LAES-INFIL                                               
712004                                                                          
720004     PERFORM UNTIL END-OF-INFIL                                           
721004        MOVE IN-IDDC TO WS-IDDC                                           
730006        IF NDC                                                            
731030           PERFORM C-KOLLA-DISTRIKT                                       
732030           IF WS-DISTR-SVAR = 311                                         
733030              CONTINUE                                                    
734031           ELSE                                                           
740030              IF IN-IDARTNR = WS-IDARTNR-SPAR                             
750030                 PERFORM B-SUMMERA-UTPOST                                 
751030              ELSE                                                        
751130                 IF WS-IDARTNR-SPAR = ZERO                                
751230                    CONTINUE                                              
751330                 ELSE                                                     
752030                    PERFORM S11-SKRIV-UTPOST                              
752140                    PERFORM S02-NOLLSTALL-UTPOST                          
752230                 END-IF                                                   
753030                 PERFORM B-SUMMERA-UTPOST                                 
754030                 MOVE IN-IDARTNR TO WS-IDARTNR-SPAR                       
755030              END-IF                                                      
756030           END-IF                                                         
790004        END-IF                                                            
790104                                                                          
791006        PERFORM S01-LAES-INFIL                                            
800000     END-PERFORM                                                          
810000                                                                          
811004     IF UT-IDARTNR NOT = ZERO                                             
812004        PERFORM S11-SKRIV-UTPOST                                          
813004     END-IF                                                               
820000                                                                          
830000     PERFORM Z-FINIT                                                      
840000                                                                          
850000     MOVE ZERO TO RETURN-CODE                                             
860000     GOBACK                                                               
870000     .                                                                    
880000     EJECT                                                                
890000 A-INIT SECTION.                                                          
900100                                                                          
901004     OPEN INPUT  INFIL                                                    
903012                 DISTRKOLL                                                
911004     OPEN OUTPUT UTFIL                                                    
911104                                                                          
911304     ACCEPT DAGENS-DATUM FROM DATE                                        
911404     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
912004                                                                          
914004     PERFORM S02-NOLLSTALL-UTPOST                                         
919204     MOVE ZERO TO WS-IDARTNR-SPAR                                         
919314                                                                          
919414     MOVE +1 TO TAB-IX                                                    
919514     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
919614        MOVE ZERO TO TAB-FOM (TAB-IX)                                     
919714                     TAB-TOM (TAB-IX)                                     
919814                     TAB-SVAR(TAB-IX)                                     
919914        ADD +1 TO TAB-IX                                                  
920014     END-PERFORM                                                          
920114                                                                          
920214     MOVE +1 TO TAB-IX                                                    
921014     PERFORM UNTIL END-OF-DISTRKOLL                                       
922016        PERFORM AA-LAS-DISTRKOLL-TILL-TAB                                 
923014        ADD +1 TO TAB-IX                                                  
924014        IF TAB-IX > TAB-IX-MAX                                            
925014           SET END-OF-DISTRKOLL TO TRUE                                   
926014        END-IF                                                            
927014     END-PERFORM                                                          
928014     .                                                                    
929014     EJECT                                                                
929114 AA-LAS-DISTRKOLL-TILL-TAB SECTION.                                       
929214                                                                          
929316     READ DISTRKOLL INTO DISTR(TAB-IX)                                    
929514        AT END SET END-OF-DISTRKOLL TO TRUE                               
929714     END-READ                                                             
950000     .                                                                    
960000     EJECT                                                                
961004 B-SUMMERA-UTPOST SECTION.                                                
961104                                                                          
964630     MOVE IN-IDDC    TO UT-IDDC                                           
964640     MOVE IN-IDARTNR TO UT-IDARTNR                                        
964730     ADD IN-REINKORD TO UT-SUINKORD                                       
964830     ADD IN-REFYSAVV TO UT-SUFYSAVP                                       
964930     ADD IN-REAVBRAD TO UT-SUAVBRP                                        
965030     ADD IN-RELAGERB TO UT-SULAGERB                                       
965130     ADD IN-RESORTB  TO UT-SUSORTB                                        
965526     .                                                                    
966004     EJECT                                                                
967030 C-KOLLA-DISTRIKT SECTION.                                                
967130                                                                          
968030     MOVE +1 TO TAB-IX                                                    
969030     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
969130        IF IN-IDDISTR >= TAB-FOM(TAB-IX)                                  
969230        AND IN-IDDISTR <= TAB-TOM(TAB-IX)                                 
969330           MOVE TAB-SVAR(TAB-IX) TO WS-DISTR-SVAR                         
969430           MOVE TAB-IX-MAX TO TAB-IX                                      
969530        END-IF                                                            
969630        ADD +1 TO TAB-IX                                                  
969730     END-PERFORM                                                          
969830     .                                                                    
969930     EJECT                                                                
970000 Z-FINIT SECTION.                                                         
980000                                                                          
980104     CLOSE INFIL                                                          
982012           DISTRKOLL                                                      
983012           UTFIL                                                          
990100                                                                          
990200     MOVE 'S' TO POSTSUM-OPKOD                                            
991000     CALL POSTSUM USING POSTSUM-PARM                                      
000000     .                                                                    
010100     EJECT                                                                
010200 S01-LAES-INFIL SECTION.                                                  
010300                                                                          
010400     READ INFIL INTO IN-AREA                                              
010500     AT END                                                               
010704        SET END-OF-INFIL TO TRUE                                          
010900     NOT AT END                                                           
011000        MOVE 'INFIL '   TO POSTSUM-FDNAMN                                 
011100        MOVE 'W23194D1' TO POSTSUM-DDNAMN2                                
011400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
011500        CALL POSTSUM USING POSTSUM-PARM                                   
011600     END-READ                                                             
012000     .                                                                    
020100     EJECT                                                                
020204 S02-NOLLSTALL-UTPOST SECTION.                                            
020304                                                                          
020404     MOVE ZERO          TO UT-IDARTNR                                     
020509                           UT-SUINKORD                                    
020609                           UT-SUFYSAVP                                    
020709                           UT-SUAVBRP                                     
021109                           UT-SULAGERB                                    
021209                           UT-SUSORTB                                     
021604     .                                                                    
021704     EJECT                                                                
021806 S11-SKRIV-UTPOST SECTION.                                                
021904                                                                          
022004     WRITE UT-POST FROM UT-AREA                                           
022104                                                                          
022204     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
022304     MOVE 'W23194'   TO POSTSUM-FDNAMN                                    
022404     MOVE 'W23194D2' TO POSTSUM-DDNAMN2                                   
022504     CALL POSTSUM USING POSTSUM-PARM                                      
023004     .                                                                    
