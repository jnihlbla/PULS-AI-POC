001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4633600.                                                
001200 AUTHOR.         BO SVENSSON.                                             
001300 DATE-WRITTEN.   98/04/15.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        BEHANDLAR FAKTURERINGSTRANSAR FRÅN PIE,                          
001900*        SKAPAR TRANS SOM SKALL TILL AUTOMATPACKNING I PULS,              
002100*                                                                         
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- FIL FRÅN PIE                                               
003503     SELECT W46335                     ASSIGN TO W46336D1.                
003504     SKIP2                                                                
003505*          --- TRANSAR TILL AUTOMATPACKNING                               
003506     SELECT W46336                     ASSIGN TO W46336D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W46335                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105                                                                          
004106*01  -COPY W46335      -L.                                                
004107     SKIP3                                                                
004108 FD  W46336                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111                                                                          
004112*01  POST -COPY W46336 -PRE UT-  -L.                                      
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4633600'.            
004510 77  WS-UTIX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
004520 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W46335-EOF-SW               PIC X       VALUE 'N'.                   
004910     88  END-OF-W46335                       VALUE 'J'.                   
004920                                                                          
004930 01  WS-IDORDNR7                 PIC 9(7)    VALUE ZERO.                  
004940 01  FILLER REDEFINES WS-IDORDNR7.                                        
004950     03  FILLER                  PIC X(2).                                
004960     03  WS-IDORDNR5             PIC X(5).                                
004961                                                                          
004962 01  WS-IDDISTR-X.                                                        
004963     03  WS-IDDISTR-N            PIC 9(4).                                
004970                                                                          
004971 01  WS-KVLEVART-X.                                                       
004972     03  WS-KVLEVART-N           PIC 9(6).                                
004973                                                                          
004980 01  WS-DADAT.                                                            
004990     03  WS-SEKEL                PIC X(2)    VALUE ZERO.                  
004991     03  WS-TIDAT                PIC 9(6)    VALUE ZERO.                  
004992 01  WS-DADAT-N REDEFINES WS-DADAT                                        
004993                                 PIC 9(8).                                
004994                                                                          
004995 01  WS-DAPRLIST-9KOMPL          PIC 9(8).                                
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005610*      --- VALID IDDC CODES                                               
005620*                                                                         
005630*01    -COPY WWDCKONS                                                     
005640       EJECT                                                              
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006220     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300     EJECT                                                                
007305 01  FILLER                     PIC X(16)   VALUE 'CIA-AREA'.             
007306*                                                                         
007307*   -COPY W009CIA                                                         
007308*                                                                         
007309     EJECT                                                                
007310*    --- PARAMETRAR TILL POSTSUM                                          
007311*                                                                         
007320*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502 01  IN-AREA-START               PIC X(24)   VALUE                        
007503                                 'IN-AREA-START  '.                       
007504     SKIP2                                                                
007505                                                                          
007506*01  AREA -COPY W46335     -PRE IN-                                       
007507     EJECT                                                                
007508 01  UT-AREA-START               PIC X(24)   VALUE                        
007509                                 'UT-AREA-START  '.                       
007510     SKIP2                                                                
007511                                                                          
007512*01  AREA -COPY W46336    -PRE UT-                                        
007600     EJECT                                                                
007700                                                                          
011003 PROCEDURE DIVISION.                                                      
011005 MAIN SECTION.                                                            
011300                                                                          
011400     PERFORM A-INIT                                                       
011500                                                                          
011610     PERFORM S01-LAES-W46335                                              
011611                                                                          
011620     IF  NOT END-OF-W46335                                                
011630         PERFORM B-INIT-PACKTRANS                                         
011650                                                                          
011700         PERFORM UNTIL END-OF-W46335                                      
011710           IF IN-IDPRODNR NOT = UT-IDPRODNR                               
011720           OR WS-UTIX        >= 15                                        
011730               PERFORM S11-SKRIV-W46336                                   
011740               PERFORM B-INIT-PACKTRANS                                   
011750           END-IF                                                         
011760                                                                          
011770           PERFORM C-KOMPL-PACKTRANS                                      
011800                                                                          
012410           PERFORM S01-LAES-W46335                                        
012500         END-PERFORM                                                      
012600                                                                          
012610         PERFORM S11-SKRIV-W46336                                         
012620     END-IF                                                               
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013501                                                                          
013510     OPEN INPUT  W46335                                                   
013601                                                                          
013602     OPEN OUTPUT W46336                                                   
013700                                                                          
013800     ACCEPT DAGENS-DATUM  FROM DATE                                       
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013911                                                                          
013920     MOVE ZERO  TO WS-UTIX                                                
014100     .                                                                    
014136     EJECT                                                                
014137 B-INIT-PACKTRANS SECTION.                                                
014138                                                                          
014139     MOVE ALL '+'       TO UT-W46336                                      
014140                                                                          
014141     MOVE IN-IDPRODNR   TO UT-IDPRODNR                                    
014143     MOVE IN-IDDISTR    TO UT-IDDISTR                                     
014144     MOVE IN-IDKUNDNR   TO UT-IDKUNDNR                                    
014145     MOVE IN-IDORDNR7   TO WS-IDORDNR7                                    
014146     MOVE WS-IDORDNR5   TO UT-IDORDNR                                     
014147     MOVE '01441'       TO UT-IDANSTNR                                    
014148     MOVE ZERO          TO UT-IDFAKT-GNB                                  
014149     MOVE 'PIE P'       TO UT-IDSNDNOD                                    
014150     MOVE ZERO          TO WS-UTIX                                        
014177     MOVE WC-CDC-SE     TO UT-IDDC                                        
014181     .                                                                    
014182     EJECT                                                                
014183 C-KOMPL-PACKTRANS SECTION.                                               
014184                                                                          
014185     MOVE IN-IDARTBET       TO CIA-IDARTBET-IN                            
014186     MOVE IN-IDARTPRE       TO CIA-IDARTPRE-IN                            
014187     CALL W009CIA USING CIA-W009CIA                                       
014189     IF CIA-KDSVAR = 'F'                                                  
014190       MOVE 'FEL FRÅN W009CIA ' TO FELTEXT-STR                            
014191       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
014194     ELSE                                                                 
014195       CONTINUE                                                           
014205     END-IF                                                               
014206                                                                          
014207     ADD  +1            TO WS-UTIX                                        
014208     IF  WS-UTIX > 15                                                     
014209       MOVE 'UTIX > 15' TO FELTEXT-STR                                    
014210       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
014212     END-IF                                                               
014213                                                                          
014214     MOVE IN-IDRADNR    TO UT-IDRADNR(WS-UTIX)                            
014215     MOVE IN-KVLEVART   TO WS-KVLEVART-N                                  
014220     MOVE WS-KVLEVART-X TO UT-KVLEVART(WS-UTIX)                           
014230     MOVE IN-IDKLIENT   TO UT-IDKLIENT(WS-UTIX)                           
014240     MOVE IN-IDARBREF   TO UT-IDARBREF(WS-UTIX)                           
014250     MOVE IN-IDBIL      TO UT-IDBIL(WS-UTIX)                              
014260     MOVE IN-IDVIN      TO UT-IDVIN(WS-UTIX)                              
014300     .                                                                    
014340     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014401     CLOSE W46335                                                         
014402           W46336                                                         
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014701     EJECT                                                                
014702 S01-LAES-W46335  SECTION.                                                
014703     READ W46335 INTO IN-AREA                                             
014704     AT END                                                               
014705        MOVE HIGH-VALUE TO IN-AREA                                        
014706        SET END-OF-W46335 TO TRUE                                         
014707                                                                          
014708     NOT AT END                                                           
014709        MOVE 'W46335' TO POSTSUM-FDNAMN                                   
014710        MOVE 'W46336D1' TO POSTSUM-DDNAMN2                                
014713        MOVE SPACE TO POSTSUM-TRANSTYP                                    
014714        CALL POSTSUM USING POSTSUM-PARM                                   
014715     END-READ                                                             
014720     .                                                                    
014801     EJECT                                                                
014802 S11-SKRIV-W46336 SECTION.                                                
014803                                                                          
014804     WRITE UT-POST FROM UT-AREA                                           
014805                                                                          
014806     MOVE SPACE TO POSTSUM-TRANSTYP                                       
014807     MOVE 'W46336' TO POSTSUM-FDNAMN                                      
014808     MOVE 'W46336D2' TO POSTSUM-DDNAMN2                                   
014809     CALL POSTSUM USING POSTSUM-PARM                                      
014810     .                                                                    
015000     EJECT                                                                
