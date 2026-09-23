000010*** EDIT ALLOWED                                                          
000001 F63-SKAPA-TULLREST SECTION.                                              
000002                                                                          
000003     MOVE SPAR-KEY-IDDISTR TO TEST-IDDISTR                                
000004                                                                          
000005     IF W-BILDA-POST                                                      
000006         AND NOT DIST03-SVERIGE                                           
000007         AND SPAR-KEY-KDCLAGER = +1                                       
000008         AND (SPAR-KEY-KDFAKTYP = 'R' OR 'G' OR 'K')                      
000009         AND F631-HELP-FLDIRLEV = 'N'                                     
000010         AND F631-HELP-KDTULLRE > +0                                      
000011         IF SPAR-FAKT-IDSKEPPN = 9 OR 99                                  
000012*           SKEPPNINGSNUMMER 9 OCH 99 AQNVÄNDS FÖR INTERN                 
000013*           UPPPACKNING OCH SKALL EJ SKICKA STATISTIK TILL                
000014*           TULL ELLER TULLRESTITUTION  (FIL W4758F)                      
000015              MOVE SPACE       TO TULLREST-UTAREA                         
000016         ELSE                                                             
000017              MOVE '010'                TO TREST-IDPTYP                   
000018              MOVE F63-RAD-IDARTNR      TO TREST-IDARTNR                  
000019              MOVE SPAR-KEY-IDDISTR     TO W-IDDISTR                      
000020              MOVE W-IDDISTR-DEL        TO TREST-IDDISTR                  
000021              MOVE SPAR-KEY-IDFAKT      TO TREST-IDFAKT                   
000022              MOVE SPAR-FAKT-TIFAKT     TO TREST-TIFAKT                   
000023              MOVE F631-HELP-KDTULLRE   TO TREST-KDTULLRE                 
000024              MOVE F63-RAD-KVLEVART     TO TREST-KVLEVART                 
000025                                                                          
000026              WRITE TULLR-POST FROM TULLREST-UTAREA                       
000027                                                                          
000028              MOVE 'W4758F'           TO POSTSUM-FDNAMN                   
000029              MOVE 'TREST   '         TO POSTSUM-DDNAMN2                  
000030              MOVE '010'              TO POSTSUM-TRANSTYP                 
000031                                                                          
000032              CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2               
000040         END-IF                                                           
000100     END-IF.                                                              
