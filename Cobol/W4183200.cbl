001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4183200.                                                
001400 AUTHOR.         SUSANNE OLSSON.                                          
001500 DATE-WRITTEN.   02/04/23.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET ÄR EN BMP SOM TAR EMOT TRANSAR FRÅN BILLIT,           
002100*        VIA WZ01-MODULEN. SKAPAR EN UTFIL MED DATA SOM SKALL             
002200*        VIDARE TILL PGM W4183400 FÖR BEARBETNING.                        
002300*        IDPTYP 32A = RADDATA                                             
002301*        IDPTYP 32B = LANDINGCOST/RAD                                     
002302*        IDPTYP 32C = TILLÄGGSKOSTNADER (FRAKT,FOERS,LEGKOST)             
002310*                                                                         
002400*    INDATA.                                                              
002410*        TRANSAKTION: W41832T                                             
002420*        MID:         WF2105 (VIA WZ01)                                   
002500*                                                                         
002510*    E-TRACKER 1752877 DATUM 20050215                                     
002511*    E-TRACKER 3181138 DATUM 20060406                                     
002520*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- KREDITNOTARADER FRÅN BILL-IT                               
003310     SELECT W41832                     ASSIGN TO W41832D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W41832                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  W41832-POST   -COPY W41832   -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4183200'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
004600     SKIP2                                                                
004700 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005710     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006010 01 WS-DATUM-8                   PIC 9(8)    VALUE ZERO.                  
006020 01  FILLER REDEFINES WS-DATUM-8.                                         
006030     03  WS-SEKEL                PIC 9(2).                                
006040     03  WS-TIAAMMDD             PIC 9(6).                                
006060     EJECT                                                                
006070     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006501     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006510     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
006520     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006601     EJECT                                                                
006602*    --- PARAMETRAR TILL POSTSUM                                          
006603*                                                                         
006610*01  -COPY W0005   -PRE  POSTSUM-                                         
006801     EJECT                                                                
006820*    --- AREOR FÖR KOMMUNIKATION                                          
006830 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
006840*01  -COPY WZ01RECV                                                       
006850     SKIP3                                                                
006860 01  RECV-DATA.                                                           
006870*03  -COPY  WZ01REQU                                                      
006880*03  -COPY  WF2105  -PRE MID-                                             
006890                                                                          
006900 01  KDRC-DISPLAY                PIC Z(5).                                
006901                                                                          
006902     SKIP3                                                                
006903 01  DECAREA.                                                             
006904* 03  WDECAREA   -COPY WDECAREA                                           
006906     EJECT                                                                
006907 01  UT-AREA-START               PIC X(24)   VALUE                        
006908                                             'UT-AREA-START'.             
006909     SKIP2                                                                
006910                                                                          
006920*01  AREA -COPY W41832     -PRE UT-                                       
007000*                                                                         
007191     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     EJECT                                                                
010801 PROCEDURE DIVISION.                                                      
010802 MAIN SECTION.                                                            
011010                                                                          
011110     PERFORM S01-RECV-OPEN                                                
011120     PERFORM S02-RECV-MESSAGE                                             
011200     PERFORM A-INIT                                                       
011410     PERFORM UNTIL RECV-KDRC > ZERO                                       
011430       PERFORM B-BEHANDLA-POSTER                                          
011440       PERFORM S02-RECV-MESSAGE                                           
011450     END-PERFORM                                                          
011460     PERFORM S03-RECV-CLOSE                                               
012400                                                                          
012500     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013200     SKIP2                                                                
013401                                                                          
013410     OPEN OUTPUT W41832                                                   
013600                                                                          
013800                                                                          
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014310 B-BEHANDLA-POSTER SECTION.                                               
014320                                                                          
014330     IF MID-IDARTNR-FINANCE = SPACE                                       
014331       IF MID-BEART = 'LANDING COST             '                         
014332         MOVE '32B'                 TO UT-IDPTYP                          
014334         MOVE MID-PRARTNTO          TO UT-PRLANDCO-RAD                    
014335         MOVE ZERO                  TO UT-PRFRAKT                         
014336                                       UT-PRLEGKST                        
014337                                       UT-PRFOERS                         
014338         PERFORM BA-FLYTTA-TKOST                                          
014339         PERFORM S11-SKRIV-W41832                                         
014340       END-IF                                                             
014341       IF MID-BEART = 'FREIGHT COST             '                         
014342         MOVE '32C'                 TO UT-IDPTYP                          
014343         MOVE ZERO                  TO UT-PRLANDCO-RAD                    
014344         MOVE MID-PRARTNTO          TO UT-PRFRAKT                         
014345         MOVE ZERO                  TO UT-PRLEGKST                        
014346                                       UT-PRFOERS                         
014350         PERFORM BA-FLYTTA-TKOST                                          
014351         PERFORM S11-SKRIV-W41832                                         
014352       END-IF                                                             
014353       IF MID-BEART = 'INSURANCE COST           '                         
014354         MOVE '32C'                 TO UT-IDPTYP                          
014355         MOVE ZERO                  TO UT-PRLANDCO-RAD                    
014356         MOVE MID-PRARTNTO          TO UT-PRFOERS                         
014357         MOVE ZERO                  TO UT-PRLEGKST                        
014358                                       UT-PRFRAKT                         
014359         PERFORM BA-FLYTTA-TKOST                                          
014360         PERFORM S11-SKRIV-W41832                                         
014361       END-IF                                                             
014362       IF MID-BEART = 'LEGALIZATION FEE         '                         
014363         MOVE '32C'                 TO UT-IDPTYP                          
014364         MOVE ZERO                  TO UT-PRLANDCO-RAD                    
014365         MOVE MID-PRARTNTO          TO UT-PRLEGKST                        
014366         MOVE ZERO                  TO UT-PRFOERS                         
014367                                       UT-PRFRAKT                         
014368         PERFORM BA-FLYTTA-TKOST                                          
014369         PERFORM S11-SKRIV-W41832                                         
014370       END-IF                                                             
014371     ELSE                                                                 
014373       PERFORM BB-FLYTTA-RADPOST                                          
014375       PERFORM S11-SKRIV-W41832                                           
014390     END-IF                                                               
014402                                                                          
014404     .                                                                    
014405     EJECT                                                                
014431 BA-FLYTTA-TKOST SECTION.                                                 
014432                                                                          
014433     IF MID-IDEXCUST-1 = SPACE                                            
014434       MOVE ZERO                    TO UT-IDDISTR                         
014435     ELSE                                                                 
014436       MOVE +0  TO W-ANT                                                  
014437       INSPECT MID-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS               
014438               BEFORE INITIAL ' '                                         
014439       MOVE MID-IDEXCUST-1(1:W-ANT) TO UT-IDDISTR                         
014440     END-IF                                                               
014441                                                                          
014442     IF MID-IDEXCUST-2 = SPACE                                            
014443       MOVE ZERO                    TO UT-IDKUNDNR                        
014444     ELSE                                                                 
014445       MOVE +0  TO W-ANT                                                  
014446       INSPECT MID-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS               
014447               BEFORE INITIAL ' '                                         
014448       MOVE MID-IDEXCUST-2(1:W-ANT) TO UT-IDKUNDNR                        
014449     END-IF                                                               
014450                                                                          
014451     IF MID-IDREF = SPACE                                                 
014452       MOVE ZERO                    TO UT-IDRAPPNR                        
014453     ELSE                                                                 
014454       MOVE +0  TO W-ANT                                                  
014455       INSPECT MID-IDREF      TALLYING W-ANT FOR CHARACTERS               
014456               BEFORE INITIAL ' '                                         
014457       MOVE MID-IDREF(1:W-ANT)      TO UT-IDRAPPNR                        
014458     END-IF                                                               
014459                                                                          
014460     MOVE MID-IDDC                  TO UT-IDDC                            
014461     MOVE MID-IDFINDOC              TO UT-IDKNOTNR                        
014462     MOVE MID-DAFINDOC              TO WS-DATUM-8                         
014463     MOVE WS-TIAAMMDD               TO UT-TIKNOTA                         
014464                                                                          
014465     IF MID-IDOPTION-2 = SPACE                                            
014466       MOVE ZERO                    TO UT-IDARTNR                         
014467     ELSE                                                                 
014468       MOVE +0  TO W-ANT                                                  
014469       INSPECT MID-IDOPTION-2  TALLYING W-ANT FOR CHARACTERS              
014470               BEFORE INITIAL ' '                                         
014471       MOVE MID-IDOPTION-2(1:W-ANT) TO UT-IDARTNR                         
014472     END-IF                                                               
014473                                                                          
014474     IF MID-IDOPTION-1 = SPACE                                            
014475       MOVE ZERO                    TO UT-IDRADNR                         
014476     ELSE                                                                 
014477       MOVE +0  TO W-ANT                                                  
014478       INSPECT MID-IDOPTION-1  TALLYING W-ANT FOR CHARACTERS              
014479               BEFORE INITIAL ' '                                         
014480       MOVE MID-IDOPTION-1(1:W-ANT) TO UT-IDRADNR                         
014481     END-IF                                                               
014482                                                                          
014483     MOVE MID-BEART                 TO UT-BEART                           
014484                                                                          
014485*- MOMSKOD BÖR VARA SPACE HÄR.                                            
014486     MOVE MID-KDVAT                 TO UT-KDVAT                           
014487     MOVE MID-KDVALISO              TO UT-KDVALISO                        
014488     MOVE MID-KDVALISO-BET          TO UT-KDVALISO-BET                    
014491     MOVE MID-PRKURS                TO UT-PRKURS                          
014492     MOVE MID-PRKURS-BET            TO UT-PRKURS-BET                      
014493     MOVE MID-PRKURS-FAKBET         TO UT-PRKURS-FAKBET                   
014494     MOVE ZERO                      TO UT-KVKREANT                        
014495     MOVE ZERO                      TO UT-PRARTNTO                        
014496     MOVE ZERO                      TO UT-SULNELOC                        
014497     MOVE ZERO                      TO UT-SUVAT-LINE                      
014498     MOVE MID-SUNTO-TOT             TO UT-SUKRENTO                        
014499     MOVE MID-SUVAT-BILLIT-TOT      TO UT-SUVAT-FAKT                      
014500     MOVE MID-SUBTO-TOT             TO UT-SUKRETOT                        
014501     MOVE MID-KDTRADP               TO UT-KDTRADP                         
014502                                                                          
014503     .                                                                    
014504     EJECT                                                                
014505 BB-FLYTTA-RADPOST SECTION.                                               
014506                                                                          
014507     MOVE '32A'                       TO UT-IDPTYP                        
014508                                                                          
014509     IF MID-IDEXCUST-1 = SPACE                                            
014510       MOVE ZERO                      TO UT-IDDISTR                       
014511     ELSE                                                                 
014512       MOVE +0  TO W-ANT                                                  
014513       INSPECT MID-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS               
014514               BEFORE INITIAL ' '                                         
014515       MOVE MID-IDEXCUST-1(1:W-ANT)   TO UT-IDDISTR                       
014516     END-IF                                                               
014517                                                                          
014518     IF MID-IDEXCUST-2 = SPACE                                            
014519       MOVE ZERO                      TO UT-IDKUNDNR                      
014520     ELSE                                                                 
014521       MOVE +0  TO W-ANT                                                  
014522       INSPECT MID-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS               
014523               BEFORE INITIAL ' '                                         
014524       MOVE MID-IDEXCUST-2(1:W-ANT) TO UT-IDKUNDNR                        
014525     END-IF                                                               
014526                                                                          
014527     IF MID-IDREF = SPACE                                                 
014528       MOVE ZERO                      TO UT-IDRAPPNR                      
014529     ELSE                                                                 
014530       MOVE +0  TO W-ANT                                                  
014531       INSPECT MID-IDREF      TALLYING W-ANT FOR CHARACTERS               
014532               BEFORE INITIAL ' '                                         
014533       MOVE MID-IDREF(1:W-ANT)        TO UT-IDRAPPNR                      
014534     END-IF                                                               
014535                                                                          
014536     MOVE MID-IDDC                  TO UT-IDDC                            
014537     MOVE MID-IDFINDOC              TO UT-IDKNOTNR                        
014538     MOVE MID-DAFINDOC              TO WS-DATUM-8                         
014539     MOVE WS-TIAAMMDD               TO UT-TIKNOTA                         
014540                                                                          
014541     IF MID-IDARTNR-FINANCE = SPACE                                       
014542       MOVE ZERO                    TO UT-IDARTNR                         
014543     ELSE                                                                 
014544       MOVE +0  TO W-ANT                                                  
014545       INSPECT MID-IDARTNR-FINANCE TALLYING W-ANT FOR CHARACTERS          
014546               BEFORE INITIAL ' '                                         
014547       MOVE MID-IDARTNR-FINANCE(1:W-ANT)   TO UT-IDARTNR                  
014548     END-IF                                                               
014549                                                                          
014550     IF MID-IDOPTION-1  = SPACE                                           
014551       MOVE ZERO                      TO UT-IDRADNR                       
014552     ELSE                                                                 
014553       MOVE +0  TO W-ANT                                                  
014554       INSPECT MID-IDOPTION-1  TALLYING W-ANT FOR CHARACTERS              
014555               BEFORE INITIAL ' '                                         
014556       MOVE MID-IDOPTION-1(1:W-ANT)   TO UT-IDRADNR                       
014557     END-IF                                                               
014558                                                                          
014559     IF MID-IDOPTION-3 NOT = SPACE                                        
014564       MOVE MID-IDOPTION-3        TO DEC-IDFRIDATA                        
014565       MOVE 7                     TO DEC-KVHELTAL                         
014566       MOVE 2                     TO DEC-KVDECIMAL                        
014567       CALL WDECEDIT USING WDECAREA                                       
014568       IF DEC-KDSVAR-OK                                                   
014569          MOVE DEC-IDEDITDATA       TO UT-PRLANDCO-RAD                    
014570       ELSE                                                               
014571          MOVE +0                   TO UT-PRLANDCO-RAD                    
014572       END-IF                                                             
014573     ELSE                                                                 
014574       MOVE +0                      TO UT-PRLANDCO-RAD                    
014575     END-IF                                                               
014576                                                                          
014577     MOVE MID-BEART                 TO UT-BEART                           
014578     MOVE MID-KDVAT                 TO UT-KDVAT                           
014579                                                                          
014580*-KDVALISO = KREDITNOTANS VALUTA / KDVALISO-BET = KUNDENS VALUTA.         
014582     MOVE MID-KDVALISO              TO UT-KDVALISO                        
014583     MOVE MID-KDVALISO-BET          TO UT-KDVALISO-BET                    
014584*-PRKURS = KREDITNOTANS KURS MOT SEK, PRKURS-BET ÄR KUNDENS/BETAL-        
014585*-ARENS KURS MOT SEK.PRKURS-FAKBET ÄR BET. KURS MOT FAKTURAN.             
014586     MOVE MID-PRKURS                TO UT-PRKURS                          
014587     MOVE MID-PRKURS-BET            TO UT-PRKURS-BET                      
014588     MOVE MID-PRKURS-FAKBET         TO UT-PRKURS-FAKBET                   
014589                                                                          
014590     MOVE MID-KVLEVART              TO UT-KVKREANT                        
014591     MOVE MID-PRARTNTO              TO UT-PRARTNTO                        
014592     MOVE MID-SUNTO                 TO UT-SULNELOC                        
014593     MOVE MID-SUVAT-BILLIT          TO UT-SUVAT-LINE                      
014594     MOVE MID-SUNTO-TOT             TO UT-SUKRENTO                        
014595     MOVE MID-SUVAT-BILLIT-TOT      TO UT-SUVAT-FAKT                      
014596     MOVE MID-SUBTO-TOT             TO UT-SUKRETOT                        
014597     MOVE ZERO                      TO UT-PRFRAKT                         
014598                                       UT-PRLEGKST                        
014599                                       UT-PRFOERS                         
014600     MOVE MID-KDTRADP               TO UT-KDTRADP                         
014601                                                                          
014602     .                                                                    
014603     EJECT                                                                
014604 Z-FINIT SECTION.                                                         
014605                                                                          
014606                                                                          
014610     CLOSE W41832                                                         
014801     SKIP2                                                                
014802     MOVE 'S' TO POSTSUM-OPKOD                                            
014810     CALL POSTSUM USING POSTSUM-PARM                                      
015000     .                                                                    
015201     EJECT                                                                
015202 S01-RECV-OPEN SECTION.                                                   
015203     MOVE 'OPEN' TO RECV-KDFUNC                                           
015204     MOVE 'CARPARTS.PULS.FBCREDIT' TO RECV-ADDISPABS                      
015205                                                                          
015206     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
015207                                   RECV-OPEN-AREA                         
015208********              ...FELHANTERING...                                  
015209     IF RECV-KDRC > 0                                                     
015210       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
015211       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
015212       DELIMITED BY SIZE INTO FELTEXT-STR                                 
015213       DISPLAY FELTEXT                                                    
015214       CALL FELLOG                                                        
015215     END-IF                                                               
015216     .                                                                    
015217     EJECT                                                                
015218 S02-RECV-MESSAGE SECTION.                                                
015219                                                                          
015220     MOVE 'GET' TO RECV-KDFUNC                                            
015221     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
015222     CALL WZ01RECV USING RECV-CONTROL-AREA                                
015223                         RECV-KVDLEN                                      
015224                         RECV-DATA                                        
015225**FELHANTERING...                                                         
015226     IF RECV-KDRC > 1                                                     
015227       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
015228       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
015229       DELIMITED BY SIZE INTO FELTEXT-STR                                 
015230       DISPLAY FELTEXT                                                    
015231       CALL FELLOG                                                        
015232     END-IF                                                               
015233     .                                                                    
015234     EJECT                                                                
015235 S03-RECV-CLOSE SECTION.                                                  
015236                                                                          
015237     MOVE 'CLOSE' TO RECV-KDFUNC                                          
015238     CALL WZ01RECV USING RECV-CONTROL-AREA                                
015239**FELHANTERING...                                                         
015240     IF RECV-KDRC > 0                                                     
015241      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
015242      STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                     
015243       DELIMITED BY SIZE INTO FELTEXT-STR                                 
015244       DISPLAY FELTEXT                                                    
015245       CALL FELLOG                                                        
015246     END-IF                                                               
015247     .                                                                    
015248     EJECT                                                                
015249 S11-SKRIV-W41832 SECTION.                                                
015250     SKIP2                                                                
015251     WRITE W41832-POST FROM UT-AREA                                       
015252                                                                          
015253     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
015254     MOVE 'W41832 '  TO POSTSUM-FDNAMN                                    
015255     MOVE 'W41832D1' TO POSTSUM-DDNAMN2                                   
015256     CALL POSTSUM USING POSTSUM-PARM                                      
015260     .                                                                    
