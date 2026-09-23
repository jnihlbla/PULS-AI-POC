000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3022100.                                                
000300 AUTHOR.         BOHLIN HÅKAN.                                            
000400 DATE-WRITTEN.   12/11/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        TP PROGRAM SOM STARTAR SOP RUTIN W330S1.                         
000900*        SKICKAR MED STYR PARAMETRAR TILL SOP                             
001000*        (SELEKTERINGS VILLKOR FÖR ATT SKAPA EN MAIL RAPPORT).            
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W3T221 + W3T221U                                    
001500*        MID:         W3I22101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W3O22101                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W3022100'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003210 77  W-IDFKNGRP-INTERVAL         PIC S9(4)   VALUE ZERO.                  
003220 77  W-IDFKNGRP-FOM              PIC 9(4)    VALUE ZERO.                  
003230 77  W-IDFKNGRP-TOM              PIC 9(4)    VALUE ZERO.                  
003300                                                                          
003400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003500                                                                          
003600                                                                          
003700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003800     88  INDATA-OK                           VALUE 'J'.                   
003900     88  INDATA-FEL                          VALUE 'N'.                   
004000                                                                          
004100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004200     88  EGEN-MID                            VALUE '3221'.                
004300     88  GODK-MID                            VALUE '3221'.                
004400     88  HELP-MID                            VALUE '0551'.                
004401                                                                          
004410     EJECT                                                                
004420 01  LIST1-DATA.                                                          
004430   03  FILLER                     PIC X(5)    VALUE 'DATA('.              
004450   03  LIST1-KDPRODSL-FOM         PIC 9(2).                               
004451   03  LIST1-KDPRODSL-TOM         PIC 9(2).                               
004452   03  LIST1-IDFKNGRP-FOM         PIC 9(4).                               
004453   03  LIST1-IDFKNGRP-TOM         PIC 9(4).                               
004454   03  LIST1-IDARTNR              PIC 9(9).                               
004455   03  FILLER                     PIC X(1)    VALUE ')'.                  
004460   03  FILLER                     PIC X(4)    VALUE SPACE.                
004461                                                                          
004470 01  LIST2-DATA.                                                          
004480   03  FILLER                     PIC X(7)    VALUE 'IDUSER('.            
004490   03  LIST2-IDUSER               PIC X(8).                               
004495   03  FILLER                     PIC X(1)    VALUE ')'.                  
004496   03  FILLER                     PIC X(4)    VALUE SPACE.                
004500     EJECT                                                                
004600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
004700 01  GENERELLA-SUBPROGRAM.                                                
004800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     EJECT                                                                
005300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005400*01 -COPY WMEDAREA                                                        
005500     SKIP3                                                                
005600 01  MESSAGE-CODES.                                                       
005700     03  WRONG-SELECTION         PIC X(3)    VALUE '002'.                 
005701     03  PRESS-PF11              PIC X(3)    VALUE '003'.                 
005710     03  KEYS-MISSING            PIC X(3)    VALUE '005'.                 
005720     03  BOTH-FIELDS-REQ         PIC X(3)    VALUE '016'.                 
005730     03  NOT-NUMERIC             PIC X(3)    VALUE '020'.                 
005800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
005900     03  WRONG-INTERVAL          PIC X(3)    VALUE '738'.                 
006000                                                                          
006700 01  SPAR-AREA.                                                           
006800     03  SPAR-IDTRANS            PIC X(4)    VALUE '3221'.                
006801                                                                          
006810     EJECT                                                                
006820 01  FILLER                      PIC X(8)  VALUE 'SOP     '.              
006830*01  -COPY WMSGSOP                                                        
006840                                                                          
006900     EJECT                                                                
007000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007300     SKIP3                                                                
007400*01  MID -COPY W3I22101                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007700     SKIP3                                                                
007800*01  -COPY WMSGAREA                                                       
007900     EJECT                                                                
008000     03  MOD REDEFINES MSG-AREA.                                          
008100*      05  -COPY W3O22101                                                 
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008400     SKIP3                                                                
008500*01  -COPY WMFSAREA                                                       
008600     EJECT                                                                
008700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300*    --- STATUS KODER FRÅN IMS                                            
009400 01  STATUS-WS                   PIC X(2).                                
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     SKIP2                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900                                                                          
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200*01  -COPY W0009   -PRE MSG-                                              
011300*01  -COPY W0009   -PRE ALT-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB.                               
011700 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
011900                                                                          
012000     PERFORM IMS-GET-MSG                                                  
012100     IF SEGMENT-FINNS                                                     
012200       PERFORM A-INIT                                                     
012300       PERFORM B-KOLLA-INDATA                                             
012400       IF INDATA-OK                                                       
012410          IF MFS-UPDATE                                                   
012420            PERFORM C-STARTA-SOP-W0T606U                                  
012430            MOVE 'ROUTINE W330S1 IS ACTIVATED'  TO MOD-TEMFSINF           
012460          ELSE                                                            
012470            MOVE '003'      TO MED-IDMFSFEL                               
012480            CALL WMEDKONV USING MED-WMEDAREA                              
012490            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
012492          END-IF                                                          
012500                                                                          
012600       END-IF                                                             
012700       PERFORM MFS-ROER-EJ-FAELT-URVAL                                    
012900       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O22101 + 4                      
013000       PERFORM IMS-INSERT-MSG                                             
013100     END-IF                                                               
013200                                                                          
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013800                                                                          
013900     IF MSG-DUBBLA-TRANSKODER                                             
014000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I22101                 
014100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
014200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
014300     ELSE                                                                 
014400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I22101                  
014500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
014600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
014700     END-IF                                                               
014800                                                                          
014900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
015000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
015100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
015200                                                                          
015300     MOVE LOW-VALUE TO MSG-AREA                                           
015400     MOVE 'W3O221N1' TO MFS-IDMOD                                         
015500     MOVE '3221' TO MOD-IDTRANS                                           
015600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
015700                                                                          
015800     IF EGEN-MID OR HELP-MID                                              
015900       CONTINUE                                                           
016000     ELSE                                                                 
016100       MOVE SPACE TO MFS-KDTRTYP                                          
016200       MOVE '7' TO MFS-IDPFK                                              
016300     END-IF                                                               
016301                                                                          
016310     MOVE 'GB' TO MED-IDSKYLT                                             
016320     MOVE MSG-SIGNON-USERID TO LIST2-IDUSER                               
016330     MOVE JA TO INDATA-SW                                                 
016400     .                                                                    
016500     EJECT                                                                
016600 B-KOLLA-INDATA SECTION.                                                  
018300                                                                          
018310     IF MID-W3I22101 = ALL '+'                                            
018311        MOVE NEJ TO INDATA-SW                                             
018312        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                        
018313        MOVE KEYS-MISSING TO MED-IDMFSFEL                                 
018330     ELSE                                                                 
018331        PERFORM BA-KOLLA-KDPRODSL                                         
018332        PERFORM BB-KOLLA-IDFKNGRP                                         
018333        PERFORM BC-KOLLA-IDARTNR                                          
018334        IF INDATA-OK                                                      
018335           IF MID-IDARTNR NOT = ALL '+'                                   
018341              IF MID-KDPRODSL NOT = ALL '+'                               
018342                 MOVE NEJ TO INDATA-SW                                    
018343                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-ATTR              
018344                 MOVE WRONG-SELECTION TO MED-IDMFSFEL                     
018345              END-IF                                                      
018346              IF MID-IDFKNGRP-FOM NOT = ALL '+'                           
018347                 MOVE NEJ TO INDATA-SW                                    
018348                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR          
018349                 MOVE WRONG-SELECTION TO MED-IDMFSFEL                     
018350              END-IF                                                      
018351              IF MID-IDFKNGRP-TOM NOT = ALL '+'                           
018352                 MOVE NEJ TO INDATA-SW                                    
018353                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR          
018355                 MOVE WRONG-SELECTION TO MED-IDMFSFEL                     
018356              END-IF                                                      
018357           ELSE                                                           
018358              IF MID-KDPRODSL NOT = ALL '+'                               
018359                 IF MID-IDFKNGRP-FOM = ALL '+'                            
018360                   MOVE NEJ TO INDATA-SW                                  
018361                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR        
018362                   MOVE WRONG-SELECTION TO MED-IDMFSFEL                   
018363                 END-IF                                                   
018364              ELSE                                                        
018365                 IF MID-IDFKNGRP-FOM = ALL '+'                            
018366                   MOVE NEJ TO INDATA-SW                                  
018367                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR        
018368                   MOVE WRONG-SELECTION TO MED-IDMFSFEL                   
018369                 ELSE                                                     
018370                   MOVE ZERO TO LIST1-KDPRODSL-FOM                        
018371                   MOVE 99   TO LIST1-KDPRODSL-TOM                        
018373                 END-IF                                                   
018374              END-IF                                                      
018375           END-IF                                                         
018380        END-IF                                                            
018427     END-IF                                                               
018430                                                                          
018500     IF INDATA-FEL                                                        
018600       CALL WMEDKONV USING MED-WMEDAREA                                   
018700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
019000     END-IF                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 BA-KOLLA-KDPRODSL SECTION.                                               
019310     IF MID-KDPRODSL NOT = ALL '+'                                        
019320        IF MID-KDPRODSL NUMERIC                                           
019321           MOVE MID-KDPRODSL TO LIST1-KDPRODSL-FOM                        
019322                                LIST1-KDPRODSL-TOM                        
019323           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                  
019324        ELSE                                                              
019325           MOVE NEJ TO INDATA-SW                                          
019330           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-ATTR                  
019340           MOVE NOT-NUMERIC TO MED-IDMFSFEL                               
019400        END-IF                                                            
019401     ELSE                                                                 
019410         MOVE ZERO TO LIST1-KDPRODSL-FOM                                  
019420                      LIST1-KDPRODSL-TOM                                  
019500     END-IF                                                               
021240     .                                                                    
021241     EJECT                                                                
021242 BB-KOLLA-IDFKNGRP SECTION.                                               
021243                                                                          
021244     IF MID-IDFKNGRP-TOM = ALL '+'                                        
021245       IF MID-IDFKNGRP-FOM = ALL '+'                                      
021246         MOVE ZERO                     TO LIST1-IDFKNGRP-FOM              
021247                                          LIST1-IDFKNGRP-TOM              
021248       ELSE                                                               
021249         IF MID-IDFKNGRP-FOM NUMERIC                                      
021250           MOVE MID-IDFKNGRP-FOM       TO LIST1-IDFKNGRP-FOM              
021251                                          LIST1-IDFKNGRP-TOM              
021252           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDFKNGRP-FOM-ATTR           
021253         ELSE                                                             
021254           MOVE NEJ                    TO INDATA-SW                       
021255           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDFKNGRP-FOM-ATTR           
021256           MOVE NOT-NUMERIC            TO MED-IDMFSFEL                    
021257         END-IF                                                           
021258       END-IF                                                             
021259     ELSE                                                                 
021260       IF MID-IDFKNGRP-TOM NUMERIC                                        
021261         MOVE MFS-NUM-FAELT-RAETT      TO MOD-IDFKNGRP-TOM-ATTR           
021262         IF MID-IDFKNGRP-FOM NUMERIC                                      
021263           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDFKNGRP-FOM-ATTR           
021264           IF MID-IDFKNGRP-TOM < MID-IDFKNGRP-FOM                         
021265             MOVE NEJ                  TO INDATA-SW                       
021266             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-TOM-ATTR           
021267             MOVE WRONG-INTERVAL       TO MED-IDMFSFEL                    
021268           ELSE                                                           
021270             MOVE MID-IDFKNGRP-FOM     TO W-IDFKNGRP-FOM                  
021271             MOVE MID-IDFKNGRP-TOM     TO W-IDFKNGRP-TOM                  
021272             COMPUTE W-IDFKNGRP-INTERVAL = W-IDFKNGRP-TOM -               
021273                                           W-IDFKNGRP-FOM                 
021274             IF W-IDFKNGRP-INTERVAL < 10                                  
021275                MOVE MID-IDFKNGRP-FOM     TO LIST1-IDFKNGRP-FOM           
021276                MOVE MID-IDFKNGRP-TOM     TO LIST1-IDFKNGRP-TOM           
021277             ELSE                                                         
021278                MOVE NEJ                  TO INDATA-SW                    
021279                MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-TOM-ATTR        
021280                MOVE WRONG-INTERVAL       TO MED-IDMFSFEL                 
021281             END-IF                                                       
021282           END-IF                                                         
021283         ELSE                                                             
021284           MOVE NEJ                    TO INDATA-SW                       
021285           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDFKNGRP-FOM-ATTR           
021286           MOVE NOT-NUMERIC            TO MED-IDMFSFEL                    
021287         END-IF                                                           
021288       ELSE                                                               
021289         MOVE NEJ                      TO INDATA-SW                       
021290         MOVE MFS-NUM-FAELT-FEL        TO MOD-IDFKNGRP-TOM-ATTR           
021291         MOVE NOT-NUMERIC              TO MED-IDMFSFEL                    
021292       END-IF                                                             
021293     END-IF                                                               
021294     .                                                                    
021295     EJECT                                                                
021296 BC-KOLLA-IDARTNR SECTION.                                                
021297     IF MID-IDARTNR NOT = ALL '+'                                         
021298        IF MID-IDARTNR NUMERIC                                            
021299           MOVE MID-IDARTNR  TO LIST1-IDARTNR                             
021301           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR                   
021302        ELSE                                                              
021303           MOVE NEJ TO INDATA-SW                                          
021304           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR                   
021305           MOVE NOT-NUMERIC TO MED-IDMFSFEL                               
021306        END-IF                                                            
021307     ELSE                                                                 
021308        MOVE ZERO TO LIST1-IDARTNR                                        
021309     END-IF                                                               
021310                                                                          
021311     .                                                                    
021312     EJECT                                                                
021313 C-STARTA-SOP-W0T606U SECTION.                                            
021314                                                                          
021315     MOVE '3221'                    TO MSGSOP-IDTRANS                     
021316     MOVE '1'                       TO MSGSOP-KDMFSFOR                    
021320     MOVE 'O'                       TO MSGSOP-KDSOPFUNK                   
021400     MOVE 'W330S1  '                TO MSGSOP-IDPROCESS                   
021500     STRING LIST1-DATA LIST2-DATA                                         
021600            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
021700     PERFORM IMS-PURG-ALT-MSG                                             
021800     .                                                                    
022700     SKIP3                                                                
022800 MFS-ROER-EJ-FAELT-URVAL SECTION.                                         
022900                                                                          
023000*    --- ALLA UTDATA-FÄLT                                                 
023100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL                               
023110                               MOD-IDFKNGRP-FOM                           
023120                               MOD-IDFKNGRP-TOM                           
023130                               MOD-IDARTNR                                
023300     .                                                                    
025500     EJECT                                                                
025600* --- IMS SEKTIONER ---                                                   
025700     SKIP3                                                                
025800 IMS-GET-MSG SECTION.                                                     
025900                                                                          
026000     MOVE '  QC' TO GODK-STATUSKODER                                      
026100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
026200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500     SKIP3                                                                
026600 IMS-INSERT-MSG SECTION.                                                  
026700                                                                          
026800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
026900     MOVE SPACE TO GODK-STATUSKODER                                       
027000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
027100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027200     PERFORM IMS-STATUSKONTROLL                                           
027300     .                                                                    
027301     SKIP3                                                                
027310 IMS-PURG-ALT-MSG SECTION.                                                
027320                                                                          
027330     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
027340     MOVE SPACE TO GODK-STATUSKODER                                       
027350     CALL CBLTDLI USING PURG ALT-PCB MSGSOP-WMSGSOP                       
027360     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027370     PERFORM IMS-STATUSKONTROLL                                           
027380     .                                                                    
027400     EJECT                                                                
027500 IMS-STATUSKONTROLL SECTION.                                              
027600                                                                          
027700     SET STATUS-IX TO 1                                                   
027800     SEARCH GODK-STATUS                                                   
027900       AT END                                                             
028000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028100         DELIMITED BY SIZE INTO FELTEXT                                   
028200         CALL FELLOG                                                      
028300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028400         CONTINUE                                                         
028500     END-SEARCH                                                           
028600     .                                                                    
