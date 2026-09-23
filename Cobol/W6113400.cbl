001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6113400.                                                
001300*AUTHOR.         MÅNS SAMUELSSON.                                         
001400*DATE-WRITTEN.   93/11/18.                                                
001500                                                                          
001600*    REMARKS                                                              
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER AVISERINGS OCH LEVERANSPLANE INFO,                         
002000*        KOPLETTERAR MED ARTIKELINFO OCH                                  
002100*        ANROPAR W611STYR FÖR ATT FÅ FRAM FB OCH FP GRUPP.                
002200*        SKRIVER FIL TILL VIOS                                            
002300*                                                                         
002410*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- AVISERINGAR OCH LEVERANSAVROP I ARTNR ORDNING              
003703     SELECT W61132                     ASSIGN TO W61134D1.                
003704     SKIP2                                                                
003705*          --- FIL MED LEVERANSINFO FÖR PRUDUKTIONSPLANERING TILL         
003710     SELECT W61134                     ASSIGN TO W61134D2.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W61132                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305     SKIP2                                                                
004306*01  -COPY W6113201      -L.                                              
004307     SKIP3                                                                
004308 FD  W61134                                                               
004309     RECORDING       F                                                    
004310     BLOCK CONTAINS  0.                                                   
004311     SKIP2                                                                
004320*01  POST -COPY W6113201 -PRE  UT34-  -L.                                 
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W6113400'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005101                                                                          
005102 77  W61132-EOF-SW               PIC X       VALUE 'N'.                   
005110     88  END-OF-W61132                       VALUE 'J'.                   
005120*                                                                         
005300 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005301 01  SPAR-KDGK                   PIC S9(1)   VALUE ZERO COMP-3.           
005302 01  SPAR-IDFKNGRP               PIC S9(5)   VALUE ZERO COMP-3.           
005310 01  SPAR-BEFT                   PIC S9(3)   VALUE ZERO COMP-3.           
005320 01  SPAR-ADLAGOMR               PIC S9(3)   VALUE ZERO COMP-3.           
005330 01  SPAR-PRARTSTD               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005340 01  SPAR-VKART                  PIC S9(7)   VALUE ZERO COMP-3.           
005350 01  SPAR-VLARTNTO               PIC S9(8)V9(1) VALUE ZERO COMP-3.        
005360 01  SPAR-ADINLOMR               PIC X(4)    VALUE SPACE.                 
005800     EJECT                                                                
005810*      --- VALID IDDC CODES                                               
005820*                                                                         
005830*01    -COPY WWDCKONS                                                     
005840       EJECT                                                              
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006401     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006410     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
006500     SKIP2                                                                
006600*    --- PARAMETRAR TILL ABEND                                            
006700                                                                          
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  FELTEXT.                                                             
007200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007401     EJECT                                                                
007402*    --- PARAMETRAR TILL POSTSUM                                          
007403*                                                                         
007410*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007510*01  -COPY W611STYR                                                       
007601     EJECT                                                                
007602 01  IN32-AREA-START             PIC X(24)   VALUE                        
007603                                 'IN32-AREA-START  '.                     
007604     SKIP2                                                                
007605                                                                          
007606*01  AREA -COPY W6113201     -PRE IN32-                                   
007607     EJECT                                                                
007608 01  UT34-AREA-START             PIC X(24)   VALUE                        
007609                                 'UT34-AREA-START  '.                     
007610     SKIP2                                                                
007611                                                                          
007620*01  AREA -COPY W6113201     -PRE UT34-                                   
007700     EJECT                                                                
007800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008401     03  W-IDARTNR-X.                                                     
008402         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008403     03  W-KDCLAGER-X.                                                    
008404         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     SKIP2                                                                
009200 01  GODK-STATUSKODER.                                                    
009300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600 01  SSA2                        PIC X(64).                               
009700     EJECT                                                                
009800*    --- IMS FUNKTIONSKODER                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010400     SKIP3                                                                
010500 01  DLI-IO-AREA.                                                         
010600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
010701     SKIP3                                                                
010702     03  WLARTC01 REDEFINES IO-AREA.                                      
010703*        05  -COPY WDK601  -PRE ARTC-                                     
010704     SKIP3                                                                
010705     03  WLARTC11 REDEFINES IO-AREA.                                      
010706*        05  -COPY WDK611  -PRE ARTC-                                     
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011301     EJECT                                                                
011410*01  -COPY W0008  -PRE ARTC-                                              
011420     05  FILLER                  PIC X.                                   
011430     EJECT                                                                
011500*01  -COPY W0008  -PRE HANA-                                              
011501     05  FILLER                  PIC X.                                   
011502     EJECT                                                                
011503*01  -COPY W0008  -PRE PLAA-                                              
011504     05  FILLER                  PIC X.                                   
011505     EJECT                                                                
011506 PROCEDURE DIVISION  USING ARTC-PCB HANA-PCB PLAA-PCB.                    
011510     ENTRY 'DLITCBL' USING ARTC-PCB HANA-PCB PLAA-PCB.                    
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-LAES-W61132                                              
012100     PERFORM UNTIL END-OF-W61132                                          
012200       IF IN32-IDARTNR NOT = W-IDARTNR                                    
012300         PERFORM B-LAES-ARTIKEL-UPPGIFTER                                 
012400       END-IF                                                             
012500       PERFORM C-SKAPA-UTFIL34                                            
012810       PERFORM S01-LAES-W61132                                            
012900     END-PERFORM                                                          
013000                                                                          
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013901                                                                          
013910     OPEN INPUT  W61132                                                   
014001                                                                          
014010     OPEN OUTPUT W61134                                                   
014100     SKIP2                                                                
014310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014610 B-LAES-ARTIKEL-UPPGIFTER SECTION.                                        
014611                                                                          
014612     MOVE IN32-IDARTNR     TO W-IDARTNR                                   
014619     MOVE +1               TO W-KDCLAGER                                  
014620                                                                          
014621     PERFORM IMS-GU-ARTC01                                                
014625     MOVE ARTC-ART-IDFKNGRP TO SPAR-IDFKNGRP                              
014626                                                                          
014635     PERFORM IMS-GNP-ARTC11                                               
014636     MOVE ARTC-CLAG-PRARTSTD    TO SPAR-PRARTSTD                          
014637                                                                          
014639     MOVE ARTC-CLAG-VKART       TO SPAR-VKART                             
014640     MOVE ARTC-CLAG-VLARTNTO    TO SPAR-VLARTNTO                          
014641     MOVE ARTC-CLAG-BEFT        TO SPAR-BEFT                              
014642     MOVE ARTC-CLAG-KDGK        TO SPAR-KDGK                              
014643                                                                          
014645     MOVE ARTC-CLAG-ADLAGOMR    TO SPAR-ADLAGOMR                          
014646                                                                          
014647     IF SPAR-KDGK = +2                                                    
014648       MOVE WC-CDC-TR   TO STYR-IDDC                                      
014649                           SPAR-IDDC                                      
014650     ELSE                                                                 
014651       MOVE WC-CDC-SE   TO STYR-IDDC                                      
014652                           SPAR-IDDC                                      
014653     END-IF                                                               
014654     MOVE IN32-IDLEVNR  TO STYR-IDLEVNR                                   
014655     MOVE IN32-IDARTNR  TO STYR-IDARTNR                                   
014656     MOVE SPAR-IDFKNGRP TO STYR-IDFKNGRP                                  
014657     MOVE SPAR-BEFT     TO STYR-BEFT                                      
014658                                                                          
014659     IF STYR-BEFT > +0                                                    
014660       CALL W611STYR USING STYR-W611STYR HANA-PCB PLAA-PCB                
014661                                                                          
014662       IF STYR-KDSVAR = SPACE                                             
014663         MOVE STYR-ADINLOMR-FP TO SPAR-ADINLOMR                           
014664       ELSE                                                               
014665         MOVE 'FEL FRÅN W611STYR' TO FELTEXT-STR                          
014666         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
014667       END-IF                                                             
014668     ELSE                                                                 
014669       MOVE SPACE TO SPAR-ADINLOMR                                        
014670     END-IF                                                               
014671     .                                                                    
014672     EJECT                                                                
014673 C-SKAPA-UTFIL34 SECTION.                                                 
014674                                                                          
014675     MOVE IN32-AREA   TO UT34-AREA                                        
014676     MOVE SPAR-IDDC     TO UT34-IDDC                                      
014677     MOVE SPAR-IDFKNGRP TO UT34-IDFKNGRP                                  
014678     MOVE SPAR-BEFT     TO UT34-BEFT                                      
014679     MOVE SPAR-ADLAGOMR TO UT34-ADLAGOMR                                  
014680     MOVE SPAR-PRARTSTD TO UT34-PRARTSTD                                  
014681     MOVE SPAR-VKART    TO UT34-VKART                                     
014682     MOVE SPAR-VLARTNTO TO UT34-VLARTNTO                                  
014683     MOVE SPAR-ADINLOMR TO UT34-ADINLOMR                                  
014684                                                                          
014685     PERFORM S11-SKRIV-W61134                                             
014686     .                                                                    
014690     EJECT                                                                
014700 Z-FINIT SECTION.                                                         
014801     CLOSE W61132                                                         
014810           W61134                                                         
014901     SKIP2                                                                
014902     MOVE 'S' TO POSTSUM-OPKOD                                            
014910     CALL POSTSUM USING POSTSUM-PARM                                      
015000     .                                                                    
015101     EJECT                                                                
015102 S01-LAES-W61132  SECTION.                                                
015103     SKIP2                                                                
015104     READ W61132 INTO IN32-AREA                                           
015105     AT END                                                               
015107        SET END-OF-W61132 TO TRUE                                         
015108                                                                          
015109     NOT AT END                                                           
015110        MOVE 'W61132' TO POSTSUM-FDNAMN                                   
015111        MOVE 'W61134D1' TO POSTSUM-DDNAMN2                                
015113        CALL POSTSUM USING POSTSUM-PARM                                   
015114     END-READ                                                             
015120     .                                                                    
015201     EJECT                                                                
015202 S11-SKRIV-W61134 SECTION.                                                
015203     SKIP2                                                                
015204     WRITE UT34-POST FROM UT34-AREA                                       
015205                                                                          
015207     MOVE 'W61134' TO POSTSUM-FDNAMN                                      
015208     MOVE 'W61134D2' TO POSTSUM-DDNAMN2                                   
015209     CALL POSTSUM USING POSTSUM-PARM                                      
015210     .                                                                    
015400     EJECT                                                                
016100* --- IMS SEKTIONER ---                                                   
016200     SKIP3                                                                
016301     EJECT                                                                
016410 IMS-GU-ARTC01   SECTION.                                                 
016420     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016430          DELIMITED BY SIZE INTO SSA1                                     
016440     MOVE '  ' TO GODK-STATUSKODER                                        
016450     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
016460     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016470     PERFORM IMS-STATUSKONTROLL                                           
016480     .                                                                    
016490     EJECT                                                                
016491 IMS-GNP-ARTC11  SECTION.                                                 
016494     MOVE 'WLARTC11'          TO SSA1                                     
016495     MOVE '  ' TO GODK-STATUSKODER                                        
016496     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
016497     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016498     PERFORM IMS-STATUSKONTROLL                                           
016499     .                                                                    
016500     EJECT                                                                
016520 IMS-STATUSKONTROLL SECTION.                                              
016600     SKIP2                                                                
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GODK-STATUS                                                   
016900       AT END                                                             
017000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
017100         DISPLAY FELTEXT                                                  
017200         CALL FELLOG                                                      
017300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017400         CONTINUE                                                         
017500     END-SEARCH                                                           
017600     .                                                                    
