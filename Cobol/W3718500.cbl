000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W3718500.                                                 
001000*AUTHOR.        STENHOLM                                                  
001100*DATE-WRITTEN.  FEB  1993                                                 
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600*                                                                         
001700*           PROGRAMMET UPPDATERAR EN FIL (W37186) VILKEN                  
001800*           ANVÄNDES SOM STYR.REGS DISTIKTSLISTA I W371P085.              
001900*           NYA TRANSAR KAN VARA AV TRE SLAG: N = TILLÄGG                 
002000*                                             D = BORTTAG                 
002100*                                             C = JUSTERING               
002200*                                                                         
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*                                                                         
002600*        U0016    - OM RETURKOD FRÅN SORT                                 
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - INFIL:                                           
003500*                        - -  FIL MED GAMLA DISTR-POSTER                  
003600*                                                                         
003700     SELECT W37186A                      ASSIGN TO UT-S-W37185D1.         
003800*- - - - - - - - - - - - INFIL:                                           
003900*                        - -  FIL MED NYA STYRREGS DISTRIKTSPOSTEW        
004000*                                                                         
004100     SELECT W37184                       ASSIGN TO UT-S-W37185D2.         
004200     SKIP2                                                                
004300*- - - - - - - - - - - - UTFIL:                                           
004400*                        - -  FIL MED       DISTR-POSTER                  
004500*                                                                         
004600     SELECT W37186-UT                    ASSIGN TO UT-S-W37185D3.         
004700     SKIP2                                                                
005600*- - - - - - - - - - - - LISTFIL:                                         
005700*                        - -  FELLISTA                                    
005800     SELECT W37187-FEL                   ASSIGN TO UT-S-W37185D4.         
005900     SKIP2                                                                
006000*- - - - - - - - - - - - SORTFIL:                                         
006100     SELECT SORTFIL                      ASSIGN TO UT-S-W37185DS.         
006200     EJECT                                                                
006300 DATA DIVISION.                                                           
006400     SKIP2                                                                
006500 FILE SECTION.                                                            
006600     SKIP3                                                                
006700 FD  W37186A                                                              
006800     RECORDING      F                                                     
006900     BLOCK CONTAINS 0.                                                    
007000     SKIP2                                                                
007100*    -COPY W37186       -L.                                               
007200     SKIP3                                                                
007300 FD  W37184                                                               
007400     RECORDING      F                                                     
007500     BLOCK CONTAINS 0.                                                    
007600     SKIP2                                                                
007700*    -COPY W371861      -L.                                               
007800     SKIP2                                                                
007900 FD  W37186-UT                                                            
008000     RECORDING      F                                                     
008100     BLOCK CONTAINS 0.                                                    
008200     SKIP2                                                                
008300*01  POST     -COPY W37186      -PRE UT-  -L.                             
008400     SKIP2                                                                
009300 FD  W37187-FEL                                                           
009400     RECORDING       F                                                    
009500     BLOCK CONTAINS 0.                                                    
009510     SKIP2                                                                
009520*01  POST     -COPY W371861     -PRE FEL-  -L.                            
009530     SKIP2                                                                
009700     EJECT                                                                
009800 SD  SORTFIL                                                              
009900                .                                                         
010000*01  POST   -COPY W371861    -PRE SORT-                                   
010100     EJECT                                                                
010200 WORKING-STORAGE SECTION.                                                 
010210                                                                          
010300*    -- CHECKED BY WY2000                                                 
011000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3718500'.               
011100     SKIP2                                                                
011200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
011300                                                                          
011400 77  JA                          PIC X(1)    VALUE 'J'.                   
011500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
011600 77  TILLAGG                     PIC X(1)    VALUE 'N'.                   
011700 77  BORTTAG                     PIC X(1)    VALUE 'D'.                   
011800 77  JUSTERING                   PIC X(1)    VALUE 'C'.                   
011900     SKIP2                                                                
012000*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
012100                                                                          
012200 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
012300     SKIP2                                                                
012400 77  W37186-EOF                  PIC X(1)    VALUE 'N'.                   
012500     SKIP2                                                                
012600                                                                          
012700     EJECT                                                                
012800 01  DAGENS-DATUM                PIC 9(6).                                
012900 01  RED-DATUM                   REDEFINES DAGENS-DATUM.                  
013000   03  DAGENS-DATUM-AR           PIC 9(2).                                
013100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
013200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
013300                                                                          
013400 01  W-NY-SORTKEY.                                                        
013500   03  W-NY-IDPTYP               PIC X(3).                                
013600   03  W-NY-IDLOPNR              PIC S9(3)   COMP-3.                      
013700     EJECT                                                                
013800 01  DYNAMISKA-SUBPROGRAM.                                                
013900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
014000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
014200     SKIP3                                                                
014300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
014400                                                                          
014500 01  RETURKODER.                                                          
014600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
014700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
014800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
014900     EJECT                                                                
015000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
015100                                                                          
015200 01  FILLER                   PIC X(16) VALUE 'POSTSUM********'.          
015300*01  -COPY W0005       -PRE  POSTSUM-.                                    
015400     EJECT                                                                
015500 01  FILLER                   PIC X(16) VALUE '***NY-AREA*****'.          
015600*01  AREA  -PRE NY-  -COPY W371861                                        
015700     EJECT                                                                
015800 01  FILLER                   PIC X(16) VALUE '*GAMM-AREA*****'.          
015900*01  AREA  -PRE GAMM-  -COPY W37186                                       
016000     EJECT                                                                
016100 01  FILLER                   PIC X(16) VALUE '*UPPD-AREA*****'.          
016200*01  AREA  -PRE UPPD-  -COPY W37186                                       
016300     EJECT                                                                
041000 PROCEDURE DIVISION.                                                      
041100     SKIP2                                                                
041200     PERFORM A-INIT                                                       
041300                                                                          
041400     SORT SORTFIL ASCENDING                                               
041500             SORT-IDPTYP                                                  
041600             SORT-IDLOPNR                                                 
041700          USING   W37184                                                  
041800          OUTPUT PROCEDURE B-BEARBETNING                                  
041900     SKIP2                                                                
042000     IF SORT-RETURN > ZERO                                                
042100       DISPLAY '***  W3718500  - FEL VID SORTERING'                       
042200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
042300     ELSE                                                                 
042400       PERFORM Z-FINIT                                                    
042500       MOVE ZERO TO RETURN-CODE                                           
042600       GOBACK                                                             
042700                                                                          
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100 A-INIT SECTION.                                                          
043200     SKIP2                                                                
043300     OPEN  INPUT W37186A                                                  
043400     OPEN OUTPUT W37187-FEL                                               
043700                 W37186-UT                                                
043800     SKIP2                                                                
043900     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
044000     SKIP2                                                                
044100     ACCEPT DAGENS-DATUM FROM DATE                                        
044200     SKIP2                                                                
045100     .                                                                    
045200     EJECT                                                                
045300 B-BEARBETNING SECTION.                                                   
045400     SKIP2                                                                
045800     PERFORM S01-LAS-NY-DISTR                                             
045900     PERFORM S02-LAS-GAMMAL-DISTR                                         
046000     PERFORM UNTIL                                                        
046100      NOT ( W37186-EOF = NEJ OR SORTFIL-EOF = NEJ )                       
046200       IF GAMM-SORTKEY < W-NY-SORTKEY                                     
046300         PERFORM S04-SKRIV-FRAN-GAMM-AREA                                 
046400         PERFORM S02-LAS-GAMMAL-DISTR                                     
046500       ELSE                                                               
046600         IF W-NY-SORTKEY < GAMM-SORTKEY                                   
046700           PERFORM BA-KOLLA-NY-DISTR                                      
046800           PERFORM S01-LAS-NY-DISTR                                       
046900         ELSE                                                             
047000           PERFORM BB-KOLLA-ANDRING                                       
047100           PERFORM S01-LAS-NY-DISTR                                       
047200           PERFORM S02-LAS-GAMMAL-DISTR                                   
047300         END-IF                                                           
047400       END-IF                                                             
047500     END-PERFORM                                                          
047600     .                                                                    
047700     EJECT                                                                
047800 BA-KOLLA-NY-DISTR   SECTION.                                             
047900     SKIP2                                                                
048000     IF NY-KDANDRING = TILLAGG                                            
048100       PERFORM S03-SKRIV-FRAN-NY-AREA                                     
048200     ELSE                                                                 
048300       PERFORM S05-SKRIV-FELLISTA                                         
048400     END-IF                                                               
048500     .                                                                    
048600     EJECT                                                                
048700 BB-KOLLA-ANDRING    SECTION.                                             
048800     SKIP2                                                                
048900     IF NY-KDANDRING = JUSTERING                                          
049000       PERFORM S03-SKRIV-FRAN-NY-AREA                                     
049100     ELSE                                                                 
049200       IF NY-KDANDRING = BORTTAG                                          
049300***************TOMSATS**********************                              
049400         CONTINUE                                                         
049500       ELSE                                                               
049600         PERFORM S05-SKRIV-FELLISTA                                       
049700         PERFORM S04-SKRIV-FRAN-GAMM-AREA                                 
049800       END-IF                                                             
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 S01-LAS-NY-DISTR        SECTION.                                         
050300     SKIP2                                                                
050400     RETURN SORTFIL   INTO    NY-AREA                                     
050500                      AT END MOVE JA TO SORTFIL-EOF                       
050600                      MOVE +99999     TO W-NY-IDPTYP                      
050700                      MOVE +999       TO W-NY-IDLOPNR                     
050800     END-RETURN                                                           
050900                                                                          
051000     IF SORTFIL-EOF = NEJ                                                 
051100                                                                          
051200       MOVE NY-IDPTYP   TO W-NY-IDPTYP                                    
051300       MOVE NY-IDLOPNR  TO W-NY-IDLOPNR                                   
051400                                                                          
051500       MOVE 'W37185'            TO POSTSUM-FDNAMN                         
051600       MOVE 'W37185D1'          TO POSTSUM-DDNAMN2                        
051700       MOVE 'NY'                TO POSTSUM-TRANSTYP                       
051800       CALL POSTSUM   USING POSTSUM-PARM                                  
051900                                                                          
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300 S02-LAS-GAMMAL-DISTR    SECTION.                                         
052400     SKIP2                                                                
052500     READ W37186A     INTO  GAMM-AREA                                     
052600                      AT END MOVE JA TO  W37186-EOF                       
052700                      MOVE +99999     TO GAMM-IDPTYP                      
052800                      MOVE +999       TO GAMM-IDLOPNR                     
052900     END-READ                                                             
053000                                                                          
053100     IF  W37186-EOF = NEJ                                                 
053200                                                                          
053300       MOVE 'W37186'            TO POSTSUM-FDNAMN                         
053400       MOVE 'W37185D2'          TO POSTSUM-DDNAMN2                        
053500       MOVE 'GAMM'              TO POSTSUM-TRANSTYP                       
053600       CALL POSTSUM   USING POSTSUM-PARM                                  
053700                                                                          
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 S03-SKRIV-FRAN-NY-AREA  SECTION.                                         
054200     SKIP2                                                                
054300     MOVE NY-IDPTYP       TO UPPD-IDPTYP                                  
054400     MOVE NY-IDLOPNR      TO UPPD-IDLOPNR                                 
054500     MOVE NY-IDDISTR-FOM  TO UPPD-IDDISTR-FOM                             
054600     MOVE NY-IDDISTR-TOM  TO UPPD-IDDISTR-TOM                             
054700     MOVE NY-IDKUNDNR-FOM TO UPPD-IDKUNDNR-FOM                            
054800     MOVE NY-IDKUNDNR-TOM TO UPPD-IDKUNDNR-TOM                            
054900     MOVE NY-IDSKYLT      TO UPPD-IDSKYLT                                 
055000     MOVE NY-KVVECKOR     TO UPPD-KVVECKOR                                
055100     MOVE NY-IDFKNGRP-FOM TO UPPD-IDFKNGRP-FOM                            
055200     MOVE NY-IDFKNGRP-TOM TO UPPD-IDFKNGRP-TOM                            
055300     MOVE NY-IDDISTR-NYTT TO UPPD-IDDISTR-NYTT                            
055400     MOVE NY-KDANDRING    TO UPPD-KDANDRING                               
055700                                                                          
055800     WRITE UT-POST FROM UPPD-AREA                                         
055900                                                                          
056000     MOVE 'W37186'            TO POSTSUM-FDNAMN                           
056100     MOVE 'W37185D3'          TO POSTSUM-DDNAMN2                          
056200     MOVE 'UT'                TO POSTSUM-TRANSTYP                         
056300     CALL POSTSUM   USING POSTSUM-PARM                                    
056310                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 S04-SKRIV-FRAN-GAMM-AREA  SECTION.                                       
056700     SKIP2                                                                
056800     WRITE UT-POST FROM GAMM-AREA                                         
056900     MOVE GAMM-IDPTYP       TO UPPD-IDPTYP                                
057000     MOVE GAMM-IDLOPNR      TO UPPD-IDLOPNR                               
057100     MOVE GAMM-IDDISTR-FOM  TO UPPD-IDDISTR-FOM                           
057200     MOVE GAMM-IDDISTR-TOM  TO UPPD-IDDISTR-TOM                           
057300     MOVE GAMM-IDKUNDNR-FOM TO UPPD-IDKUNDNR-FOM                          
057400     MOVE GAMM-IDKUNDNR-TOM TO UPPD-IDKUNDNR-TOM                          
057500     MOVE GAMM-IDSKYLT      TO UPPD-IDSKYLT                               
057600     MOVE GAMM-KVVECKOR     TO UPPD-KVVECKOR                              
057700     MOVE GAMM-IDFKNGRP-FOM TO UPPD-IDFKNGRP-FOM                          
057800     MOVE GAMM-IDFKNGRP-TOM TO UPPD-IDFKNGRP-TOM                          
057900     MOVE GAMM-IDDISTR-NYTT TO UPPD-IDDISTR-NYTT                          
058000     MOVE GAMM-KDANDRING    TO UPPD-KDANDRING                             
058300                                                                          
058400     MOVE 'W37186'            TO POSTSUM-FDNAMN                           
058500     MOVE 'W37185D3'          TO POSTSUM-DDNAMN2                          
058600     MOVE 'UT'                TO POSTSUM-TRANSTYP                         
058700     CALL POSTSUM   USING POSTSUM-PARM                                    
058710                                                                          
058800     .                                                                    
058900     EJECT                                                                
059000 S05-SKRIV-FELLISTA SECTION.                                              
059100     SKIP2                                                                
059210     WRITE FEL-POST FROM NY-AREA                                          
059300                                                                          
059400     MOVE 'W37187'            TO POSTSUM-FDNAMN                           
059500     MOVE 'W37185D4'          TO POSTSUM-DDNAMN2                          
059600     MOVE 'FEL'               TO POSTSUM-TRANSTYP                         
059700     CALL POSTSUM   USING POSTSUM-PARM                                    
059800     .                                                                    
059900     EJECT                                                                
060000 Z-FINIT SECTION.                                                         
060100     SKIP2                                                                
061900                                                                          
062300     CLOSE W37186A                                                        
062400           W37186-UT                                                      
062410           W37187-FEL                                                     
062500     SKIP2                                                                
062600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
062700*                                    SKRIVNA POSTER                       
062800                                                                          
062900     MOVE 'S' TO POSTSUM-OPKOD                                            
063000     CALL POSTSUM USING POSTSUM-PARM                                      
063100     .                                                                    
