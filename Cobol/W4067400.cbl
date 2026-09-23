000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W4067400.                                                 
000300 AUTHOR.        STINA MOGREN.                                             
000400     DATE-WRITTEN.  NOV 2004.                                             
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000610*       ( DELVIS KOPIERAD FRÅN W4059600 )                                 
000700*        PROGRAMMET BEHANDLAR TPTRANS FÖR 'LASTNINGINFO'.                 
000800*        MED HJÄLP AV DENNA KAN UPPGIFTER SOM GÄLLER HELA                 
000900*        LASTNINGEN, SÅSOM DESTINATION, BOKNINGSNR M.M, ANGES.            
001000*                                                                         
001100*        PROGRAMMET STARTAS FRÅN W4063600 (ADD-IT).                       
001300*        VISS INFORMATION SOM KAN ANGES VIA DETTA PROGRAM                 
001800*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T674                                              
002600*        MID:         W4I67401                                            
002700*    UTDATA.                                                              
002800*        MOD:         W4O67401 W0O50401                                   
002900*                     W4O66401                                            
003100*    SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  PROGRAM-NAMN        PIC X(8)        VALUE 'W4067400'.                
003800                                                                          
003900 77  I                   PIC S9(9)       COMP SYNC.                       
004000 77  KDFRAKT-WS          PIC S9(7)       COMP-3.                          
004100 77  IDSHIPM-WS          PIC X(7).                                        
004110 77  IDDC-WS             PIC X(2).                                        
004120 77  IDTRPTNR-WS         PIC X(3).                                        
004130 77  IDLBBET-WS          PIC X(12).                                       
004200 77  DESTINATION-WS      PIC X(8)        VALUE SPACE.                     
004300 77  DAGENS-DATUM        PIC X(6).                                        
004400 77  TIKLOCK             PIC X(8).                                        
004500 77  KDFRAKT             PIC 9(2).                                        
004600 77  KDTRDOK-WS          PIC S9(1)       COMP-3.                          
004610 77  W-SEKTION           PIC X(16)       VALUE SPACE.                     
004700                                                                          
004800 77  FAKTURERAD          PIC X           VALUE 'J'.                       
004900 77  JA                  PIC X           VALUE 'J'.                       
005000 77  NEJ                 PIC X           VALUE 'N'.                       
005100 77  ALFA                PIC X           VALUE 'A'.                       
005200 77  NUM                 PIC X           VALUE 'N'.                       
005300 77  FEL                 PIC X           VALUE 'F'.                       
005400 77  EJ-IFYLLD           PIC X           VALUE 'I'.                       
005500 77  RAETT               PIC X           VALUE 'R'.                       
005510 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005520     88  NYCKLAR-OK                          VALUE 'J'.                   
005530     88  NYCKLAR-FEL                         VALUE 'N'.                   
005540 77  LNG-P-TO-P-PREFIX   PIC S9(4)  VALUE +17   COMP SYNC.                
005600     SKIP2                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800   03    CBLTDLI             PIC X(8)        VALUE 'CBLTDLI '.            
005900   03    FELLOG              PIC X(8)        VALUE 'FELLOG  '.            
005910   03    W476LAST            PIC X(8)        VALUE 'W476LAST'.            
005920   03    W005INIT            PIC X(8)        VALUE 'W005INIT'.            
005930   03    WMEDKONV            PIC X(8)        VALUE 'WMEDKONV'.            
006000     EJECT                                                                
006100 01      NYCKLAR-TILL-DLI.                                                
006200   03    W-IDSHIPM-X.                                                     
006300     05  W-IDSHIPM           PIC 9(7)       VALUE ZERO.                   
006400   03    W-IDDISTR-X.                                                     
006500     05  W-IDDISTR           PIC S9(5)      VALUE ZERO  COMP-3.           
006600   03    W-IDKUNDNR-X.                                                    
006700     05  W-IDKUNDNR          PIC S9(7)      VALUE ZERO  COMP-3.           
006800   03    W-IDDC-X.                                                        
006900     05  W-IDDC              PIC  X(2)      VALUE SPACE.                  
007000   03    W-IDSKEPPN-X.                                                    
007100     05  W-IDSKEPPN          PIC S9(7)      VALUE ZERO  COMP-3.           
007400     EJECT                                                                
007500******************* MFS KONSTANTER *******************************        
007510 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007520     88  OWN-MID                             VALUE '4674'.                
007530     88  GOOD-MID                            VALUE '4636'                 
007540                                                   '4674'.                
007600                                                                          
007700 01  MFS-KONSTANTER.                                                      
007800     03  EGEN-TRANS          PIC X(4)       VALUE '4674'.                 
008100     03  EGEN-MOD            PIC X(8)       VALUE 'W4O67401'.             
008400     03  WHELP-MOD           PIC X(8)       VALUE 'W0O50401'.             
008500     03  REG-UPD-TRANSKOD    PIC X(8)       VALUE 'W4T674  '.             
008510     03  4636-TRANS          PIC X(8)       VALUE '4636'.                 
008600                                                                          
008700     03  EGEN-MOD-LL         PIC S9(4)      COMP SYNC                     
008800                                            VALUE +280.                   
009800     EJECT                                                                
009810*                                                                         
009820 01  SPAR-AREA.                                                           
009830     03  SPAR-IDTRANS           PIC X(4)    VALUE '4674'.                 
009840     03  SPAR-W4I67401-ENTER       PIC X(400).                            
009850     03  SPAR-W4I67401-NEXT        PIC X(400).                            
009860     EJECT                                                                
009900************************ FÄLTMARKERINGSAREA *******************           
010000                                                                          
010100 01      FAELT-AREA.                                                      
010200  02     FAELTTAB.                                                        
010300   03    FAELT-TIAVGANG      PIC X.                                       
010400   03    FILLER              PIC X          VALUE 'N'.                    
010500   03    FAELT-BETEXT-NEDK   PIC X.                                       
010600   03    FILLER              PIC X          VALUE 'A'.                    
010700   03    FAELT-TINEDK        PIC X.                                       
010800   03    FILLER              PIC X          VALUE 'N'.                    
010900   03    FAELT-BETEXT-HAEMT  PIC X.                                       
011000   03    FILLER              PIC X          VALUE 'A'.                    
011100   03    FAELT-TIHAEMT       PIC X.                                       
011200   03    FILLER              PIC X          VALUE 'N'.                    
011300                                                                          
011400   03    FAELT-LB-GRP.                                                    
011500     05  FILLER              PIC X(24)                                    
011600                             VALUE ' N A N A N A N A N A N A'.            
011700   03    FILLER              REDEFINES FAELT-LB-GRP.                      
011800    04   FILLER              OCCURS 6.                                    
011900     05  FAELT-KDLBTYP       PIC X.                                       
012000     05  FILLER              PIC X.                                       
012100     05  FAELT-IDLBBET       PIC X.                                       
012200     05  FILLER              PIC X.                                       
012300                                                                          
012400   03    FAELT-OEVR-GRP.                                                  
012500     05  FILLER              PIC X(6)       VALUE ' A A A'.               
012600   03    FILLER              REDEFINES FAELT-OEVR-GRP.                    
012700    04   FILLER              OCCURS 3.                                    
012800     05  FAELT-BETEXT-OEVR   PIC X.                                       
012900     05  FILLER              PIC X.                                       
013000                                                                          
013100   03    FAELT-TRDOK-GRP.                                                 
013200     05  FILLER              PIC X(6)       VALUE ' A A A'.               
013300   03    FILLER              REDEFINES FAELT-TRDOK-GRP.                   
013400    04   FILLER              OCCURS 3.                                    
013500     05  FAELT-FLTRDOK       PIC X.                                       
013600     05  FILLER              PIC X.                                       
013700     EJECT                                                                
013800   03    FAELT-BEROUTE       PIC X.                                       
013900   03    FILLER              PIC X          VALUE 'A'.                    
014000   03    FAELT-IDBOKN        PIC X.                                       
014100   03    FILLER              PIC X          VALUE 'A'.                    
014200   03    FAELT-IDTRANSP-NAMN PIC X.                                       
014300   03    FILLER              PIC X          VALUE 'A'.                    
014400                                                                          
014500  02     FILLER              REDEFINES FAELTTAB.                          
014600   03    FAELTGRP            OCCURS 26.                                   
014700     05  FAELTTAB-FAELT      PIC X.                                       
014800     05  FAELTTAB-KDDATTYP   PIC X.                                       
014900  02     FAELTTAB-IX         PIC S9(5)      COMP-3.                       
015000  02     FAELTTAB-IX-MAX     PIC S9(5)      COMP-3 VALUE +23.             
015100     EJECT                                                                
015200****************** MEDDELANDE OCH FEL-TEXTER *******************          
015300                                                                          
015400 01      MEDDELANDE-OCH-FEL.                                              
015500                                                                          
015600     03  MEDDELANDE1.                                                     
015700         05  FILLER          PIC X(79)                                    
015800             VALUE '    LASTNINGSINFO  KLAR                               
015900-                  '                                 '.                   
016000         05  FILLER          PIC X(79)                                    
016100             VALUE '    LOADING INFO  OK                                  
016200-                  '                                 '.                   
016300     03  FILLER              REDEFINES MEDDELANDE1.                       
016400         05  MEDDELANDE-1    PIC X(79)       OCCURS 2.                    
016500                                                                          
016510     03  FEL1.                                                            
016520         05  FILLER          PIC X(40)                                    
016530             VALUE '    NYCKELFEL,                         '.             
016540         05  FILLER          PIC X(40)                                    
016550             VALUE ' KEY ERROR;                            '.             
016560     03  FILLER              REDEFINES FEL1.                              
016570         05  FEL-1           PIC X(40)       OCCURS 2.                    
016580                                                                          
016600                                                                          
017700     03  FEL2.                                                            
017800         05  FILLER          PIC X(40)                                    
017900             VALUE '720 FEL BILD VALD                      '.             
018000         05  FILLER          PIC X(40)                                    
018100             VALUE '720 WRONG SCREEN SELECTED              '.             
018200     03  FILLER              REDEFINES FEL2.                              
018300         05  FEL-2           PIC X(40)      OCCURS 2.                     
018400                                                                          
018500                                                                          
018600                                                                          
018700     03  FEL3.                                                            
018800         05  FILLER           PIC X(40)                                   
018900             VALUE '702 UPPLYSTA FÄLT FEL                  '.             
019000         05  FILLER           PIC X(40)                                   
019100             VALUE '702 HIGHLIT FIELDS WRONG               '.             
019200     03  FILLER               REDEFINES FEL3.                             
019300         05  FEL-3            PIC X(40)      OCCURS 2.                    
019400     EJECT                                                                
019500 01      SWITCHAR.                                                        
019600                                                                          
019700   03    INDATA-RAETT-SW     PIC X          VALUE 'N'.                    
019800     88  INDATA-RAETT                       VALUE 'J'.                    
019900                                                                          
020000     SKIP3                                                                
020100 01      INDEX-FALT.                                                      
020200                                                                          
020300   03    SPRAAK-IX           PIC S9(3)      COMP SYNC VALUE +1.           
020400                                                                          
020500   03    IX                  PIC S9(7)      COMP SYNC VALUE ZERO.         
020600     EJECT                                                                
020601*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020602 01  FILLER                  PIC X(16)   VALUE 'WMEDAREA'.                
020603*01  -COPY WMEDAREA                                                       
020604 01  MESSAGE-CODES.                                                       
020605     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020606     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020607     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020608                                                                          
020609 01  FILLER                  PIC X(16)   VALUE 'LAST-W406710'.            
020610*01  -COPY W4067410    -PRE LAST-                                         
020620                                                                          
020630*                                                                         
020640 01  FILLER                    PIC X(16)   VALUE  'WMSGINIT'.             
020650     SKIP3                                                                
020660*01 -COPY WMSGINIT                                                        
020670     EJECT                                                                
020700*************************************************************             
020800*                                                                         
020900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
021000*                                                                         
021100 01      FILLER          PIC X(8)    VALUE 'MFS-WS  '.                    
021200     SKIP3                                                                
021400***************************   MID  **************************             
021500                                                                          
021510 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
021600*01      MID -COPY W4I67401                                               
021800     EJECT                                                                
021810 01  FILLER              PIC X(16)   VALUE 'MSG-AREA  '.                  
021900*01      -COPY WMSGAREA                                                   
022100     EJECT                                                                
022200*************** MOD   LASTNINGSINFO *************************             
022300                                                                          
022400*  03    MOD      -COPY W4O67401 -PRE MOD- -RED MSG-AREA.                 
022600     EJECT                                                                
023200********************* MOD       WHELP ***********************             
023300                                                                          
023400*  03    MOD3     -COPY W0O50401 -PRE MOD3- -RED MSG-AREA.                
023600     EJECT                                                                
023700 01  FILLER                      PIC X(16)   VALUE  'P-TO-P-AREA'.        
023800 01      P-TO-P-SW.                                                       
023900                                                                          
024000  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
024100  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
024200  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
024300  02     P-TO-P-KDTRANS          PIC X(8).                                
024400  02     P-TO-P-IDTRANS          PIC X(4).                                
024500  02     P-TO-P-KDMFSFOR         PIC X(1).                                
024600  02     P-TO-P-DATA             PIC X(1000).                             
024800*                                                                         
024810*01  -COPY W4I66401   -PRE MOD4664-                                       
024820*                                                                         
024900                                                                          
024910 01  FILLER                      PIC X(16)  VALUE 'MFS-AREA'.             
025000*01      -COPY WMFSAREA                                                   
025200     EJECT                                                                
025300*****************************************************************         
025400*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
025500*                                                                         
025600 01      IMS-WS.                                                          
025700   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
025800     SKIP3                                                                
025900*                            *** STATUSKOD FRÅN IMS                       
026000   03    STATUS-WS       PIC XX.                                          
026100     88  SEGMENT-FINNS               VALUE '  '.                          
026200     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
026300     88  SEGMENT-FINNS-REDAN         VALUE 'II'.                          
026400     88  BASEN-SLUT                  VALUE 'GB'.                          
026500     SKIP3                                                                
026600   03    GODK-STATUSKODER.                                                
026700     05  GODK-STATUS OCCURS 5    INDEXED BY STATUS-IX PIC XX.             
026800     SKIP3                                                                
026900 01      SSA1            PIC X(64).                                       
027000 01      SSA2            PIC X(64).                                       
027100     EJECT                                                                
027200*                            *** IMS FUNKTIONSKODER                       
027300*01      -COPY W0003                                                      
027500     EJECT                                                                
028000********************* TRANSPORTRELEASEREG ************************        
028100                                                                          
028110 01  FILLER                  PIC X(16) VALUE  'DLI-IO-WDE101'.            
028120 01  DLI-IO-WDE101.                                                       
028200*    03  WDE101   -COPY WDE101                                            
028210 01  DLI-IO-WDE111.                                                       
028300*    03  WDE111   -COPY WDE111                                            
028301 01  DLI-IO-WDE121.                                                       
028310*    03  WDE121   -COPY WDE121                                            
028400     EJECT                                                                
028500*01  -COPY  W476TRPD                                                      
030700*********************** LINKAGE SECTION **************************        
030800     SKIP3                                                                
030900 LINKAGE SECTION.                                                         
031000*01      -COPY W0009     -PRE MSG-                                        
031200     EJECT                                                                
031300*01      -COPY W0009     -PRE ALT1-                                       
031400     EJECT                                                                
031500*01      -COPY W0009     -PRE ALT2-                                       
031510     EJECT                                                                
031600*01      -COPY W0008     -PRE WDP7-                                       
031800      05 FILLER          PIC X.                                           
031900     EJECT                                                                
031910*01      -COPY W0008     -PRE WDE1-                                       
031920      05 FILLER          PIC X.                                           
031930     EJECT                                                                
032000*01      -COPY W0008     -PRE WDE6-                                       
032100      05 FILLER          PIC X.                                           
032200     EJECT                                                                
032300*01      -COPY W0008     -PRE WDR1-                                       
032400      05 FILLER          PIC X.                                           
032500     EJECT                                                                
032600*01      -COPY W0008     -PRE WDB9-                                       
032700      05 FILLER          PIC X.                                           
032710     EJECT                                                                
032720*01      -COPY W0008     -PRE WDB5-                                       
032730      05 FILLER          PIC X.                                           
032740     EJECT                                                                
032800********************* PROCEDURE DIVISION *************************        
032900 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB                      
032910                      WDP7-PCB                                            
033000                      WDE1-PCB WDE6-PCB                                   
033010                      WDR1-PCB WDB9-PCB WDB5-PCB.                         
033100     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB                      
033110                      WDP7-PCB                                            
033200                      WDE1-PCB WDE6-PCB                                   
033210                      WDR1-PCB WDB9-PCB WDB5-PCB.                         
033300                                                                          
033400     PERFORM IMS-GET-MSG                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM A-INIT-SPARA-INPUT                                         
033610       PERFORM B-KOLLA-NYCKLAR                                            
033700                                                                          
033810       IF MFS-IDTRANS = '4636'                                            
033900                                                                          
034000           IF (IDSHIPM-WS NOT NUMERIC) OR                                 
034010              (W-IDSHIPM NOT > ZERO)                                      
034100             PERFORM B-NYCKEL-FEL                                         
034200           ELSE                                                           
034300             PERFORM C-START-BEHANDLING                                   
034400           END-IF                                                         
034410           MOVE '36'  TO W-SEKTION(1:2)                                   
034600       ELSE                                                               
034710           IF MFS-IDTRANS = '4674'                                        
034800             IF (IDSHIPM-WS NOT NUMERIC) OR                               
034810              (W-IDSHIPM NOT > ZERO)                                      
034900               PERFORM B-NYCKEL-FEL                                       
035000             ELSE                                                         
035100               PERFORM D-KONTROLLERA-INDATA                               
035200               IF INDATA-RAETT                                            
035300                 PERFORM E-BEHANDLA-INDATA                                
035310                 PERFORM H-SKRIV-LISTA                                    
035400               ELSE                                                       
035500                 PERFORM F-INDATA-FEL                                     
035600               END-IF                                                     
035700             END-IF                                                       
035710             MOVE '74'  TO W-SEKTION(1:2)                                 
035800                                                                          
035900           ELSE                                                           
036000             PERFORM G-FELAKTIGT-ANROP                                    
036010             MOVE 'GG'  TO W-SEKTION(1:2)                                 
036100           END-IF                                                         
036200       END-IF                                                             
036300                                                                          
036301       IF NOT INDATA-RAETT                                                
036310         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O67401 + 4                    
036400         PERFORM IMS-ISRT-MSG                                             
036401       ELSE                                                               
036402         PERFORM K-STARTA-W40664                                          
036410       END-IF                                                             
036500     END-IF                                                               
036600     MOVE ZERO TO RETURN-CODE                                             
036700     GOBACK                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 A-INIT-SPARA-INPUT SECTION.                                              
037010     MOVE 'A-INIT'               TO W-SEKTION                             
037100                                                                          
037200     IF MSG-DUBBLA-TRANSKODER                                             
037300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I67401                 
037400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
037500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
037600     ELSE                                                                 
037700       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I67401                   
037800       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
037900       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
038000     END-IF                                                               
038010     MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                    
038020     MOVE MSG-IDPFK                     TO MFS-IDPFK                      
038030     MOVE MFS-IDTRANS                   TO W-IDTRANS                      
038100                                                                          
038101     MOVE LOW-VALUE                     TO MSG-AREA                       
038110     MOVE 'W4O67401'                    TO MFS-IDMOD                      
038120     MOVE '4674'                        TO MOD-FRAN-IDTRANS               
038121                                           MOD-TRANS-NUMMER               
038130     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL MOD-TEMFSINF             
038140                                                                          
038200     IF ENGLISH-TEXT                                                      
038300       MOVE +2  TO SPRAAK-IX                                              
038400     ELSE                                                                 
038500       MOVE +1  TO SPRAAK-IX                                              
038600     END-IF                                                               
038700                                                                          
038800     MOVE SPACE                  TO LAST-W4067410                         
038900     MOVE ZERO                   TO LAST-IDSHIPM                          
039000                                    LAST-IDTRPTNR                         
039200     .                                                                    
039300     EJECT                                                                
039310 B-KOLLA-NYCKLAR  SECTION.                                                
039311     MOVE 'B-KOLLA-NYCKLAR'     TO W-SEKTION                              
039320                                                                          
039330     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039340     MOVE '001'             TO MSGI-KDCALL                                
039350     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039360     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039370     MOVE '4674'            TO MSGI-IDTRANS                               
039380     IF GOOD-MID                                                          
039390         MOVE MID-IDSHIPM        TO MSGI-IDSHIPM                          
039391     END-IF                                                               
039392                                                                          
039393     IF OWN-MID                                                           
039394        CONTINUE                                                          
039395     ELSE                                                                 
039396        MOVE SPACE   TO MFS-KDTRTYP                                       
039397        MOVE '7'     TO MFS-IDPFK                                         
039398     END-IF                                                               
039399     IF GOOD-MID                                                          
039400        MOVE MID-IDTRPTNR      TO MSGI-IDTRPTNR                           
039401        MOVE MID-IDLBBET-HUV   TO MSGI-IDLBBET                            
039402        MOVE MID-IDDC          TO MSGI-IDDC-KEY                           
039403        MOVE MID-IDSHIPM       TO MSGI-IDSHIPM                            
039404     END-IF                                                               
039405     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
039406*    MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
039407                                                                          
039408*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
039409     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
039410                                                                          
039411     MOVE JA TO NYCKLAR-SW                                                
039412                                                                          
039413                                                                          
039415*    -- KONTROLL AV IDSHIPM                                               
039416     MOVE MFS-RENSA-FAELT    TO MOD-IDSHIPM                               
039417                                                                          
039418     IF MFS-IDTRANS = '4636' OR '4674'                                    
039423      IF MID-IDSHIPM    NOT = ALL '+'                                     
039424        MOVE '7'              TO MFS-IDPFK                                
039425        MOVE SPACE            TO MFS-KDTRTYP                              
039426      END-IF                                                              
039427      MOVE MID-IDSHIPM        TO IDSHIPM-WS                               
039428                                 MOD-IDSHIPM                              
039429      INSPECT IDSHIPM-WS   REPLACING LEADING SPACE BY ZERO                
039430      MOVE IDSHIPM-WS         TO W-IDSHIPM                                
039431      MOVE MID-IDDC           TO IDDC-WS                                  
039432                                 MOD-IDDC                                 
039433      INSPECT IDDC-WS      REPLACING LEADING SPACE BY ZERO                
039434      MOVE MID-IDTRPTNR       TO IDTRPTNR-WS                              
039435                                 MOD-IDTRPTNR                             
039436      INSPECT IDTRPTNR-WS  REPLACING LEADING SPACE BY ZERO                
039437      MOVE MID-IDLBBET-HUV    TO IDLBBET-WS                               
039438                                 MOD-IDLBBET-HUV                          
039439     ELSE                                                                 
039441       MOVE NEJ   TO NYCKLAR-SW                                           
039442     END-IF                                                               
039444     IF GOOD-MID OR NYCKLAR-OK                                            
039445       MOVE MSGI-IDSHIPM        TO MOD-IDSHIPM                            
039446     ELSE                                                                 
039447       MOVE MFS-RENSA-FAELT     TO MOD-IDSHIPM                            
039448     END-IF                                                               
039449                                                                          
039450     IF NYCKLAR-FEL                                                       
039451       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
039452       CALL WMEDKONV     USING MED-WMEDAREA                               
039453       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
039454*      PERFORM MFS-RENSA-FAELT-IN                                         
039455     END-IF                                                               
039457     .                                                                    
039458     EJECT                                                                
039460 B-NYCKEL-FEL SECTION.                                                    
039500     MOVE 'B-NYCK'          TO W-SEKTION                                  
039510                                                                          
039600     MOVE IDSHIPM-WS        TO MOD-IDSHIPM                                
039700     INSPECT MOD-IDSHIPM REPLACING LEADING ZERO BY SPACE                  
039800                                                                          
039900     MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                               
040000                                                                          
040100*    PERFORM MFS-NYCKEL-FEL                                               
040200     .                                                                    
040300     EJECT                                                                
040400 C-START-BEHANDLING SECTION.                                              
040500     MOVE 'C-START'         TO W-SEKTION                                  
040510                                                                          
040600     MOVE IDSHIPM-WS        TO W-IDSHIPM                                  
040700     PERFORM IMS-GU-WDE101                                                
040710     PERFORM IMS-GNP-WDE111                                               
040800                                                                          
042300     MOVE IDSHIPM-WS        TO MOD-IDSHIPM                                
042310     MOVE IDDC-WS           TO MOD-IDDC                                   
042400     INSPECT MOD-IDSHIPM REPLACING LEADING ZERO BY SPACE                  
042500*    PERFORM MFS-SVAR-START-MOD                                           
042600     MOVE '4674'            TO MOD-FRAN-IDTRANS                           
042700     .                                                                    
042800     EJECT                                                                
042900 D-KONTROLLERA-INDATA SECTION.                                            
043000     MOVE 'D-KONTR'        TO W-SEKTION                                   
043010                                                                          
043100     MOVE JA TO INDATA-RAETT-SW                                           
043300                                                                          
043400     IF MID-BEROUTE = ALL '+' OR SPACE                                    
043500       MOVE FEL     TO FAELT-BEROUTE                                      
043600       MOVE NEJ     TO INDATA-RAETT-SW                                    
043700     ELSE                                                                 
043800       MOVE RAETT   TO FAELT-BEROUTE                                      
043900     END-IF                                                               
044000                                                                          
044100     IF MID-IDBOKN = ALL '+'                                              
044200       MOVE EJ-IFYLLD TO FAELT-IDBOKN                                     
044300     ELSE                                                                 
044400       MOVE RAETT     TO FAELT-IDBOKN                                     
044500     END-IF                                                               
044600                                                                          
044700     IF MID-IDTRANSP-NAMN = ALL '+' OR SPACE                              
044800       MOVE FEL      TO FAELT-IDTRANSP-NAMN                               
044900       MOVE NEJ      TO INDATA-RAETT-SW                                   
045000     ELSE                                                                 
045100       MOVE RAETT    TO FAELT-IDTRANSP-NAMN                               
045200     END-IF                                                               
045300                                                                          
045400     IF MID-TIAVGANG = ALL '+'                                            
045500       MOVE EJ-IFYLLD TO FAELT-TIAVGANG                                   
045600     ELSE                                                                 
045700     IF MID-TIAVGANG NUMERIC                                              
045800       MOVE RAETT     TO FAELT-TIAVGANG                                   
045900     ELSE                                                                 
046000       MOVE FEL       TO FAELT-TIAVGANG                                   
046100       MOVE NEJ       TO INDATA-RAETT-SW                                  
046200     END-IF                                                               
046300     END-IF                                                               
046400                                                                          
046500     IF MID-BETEXT-NEDK = ALL '+'                                         
046600       MOVE EJ-IFYLLD TO FAELT-BETEXT-NEDK                                
046700     ELSE                                                                 
046800       MOVE RAETT     TO FAELT-BETEXT-NEDK                                
046900     END-IF                                                               
047000                                                                          
047100     IF MID-TINEDK = ALL '+'                                              
047200       MOVE EJ-IFYLLD TO FAELT-TINEDK                                     
047300     ELSE                                                                 
047400     IF MID-TINEDK NUMERIC                                                
047500       MOVE RAETT     TO FAELT-TINEDK                                     
047600     ELSE                                                                 
047700       MOVE FEL       TO FAELT-TINEDK                                     
047800       MOVE NEJ       TO INDATA-RAETT-SW                                  
047900     END-IF                                                               
048000     END-IF                                                               
048100                                                                          
048200                                                                          
048300     IF MID-BETEXT-HAEMT = ALL '+'                                        
048400       MOVE EJ-IFYLLD TO FAELT-BETEXT-HAEMT                               
048500     ELSE                                                                 
048600       MOVE RAETT     TO FAELT-BETEXT-HAEMT                               
048700     END-IF                                                               
048800                                                                          
048900                                                                          
049000     IF MID-TIHAEMT = ALL '+'                                             
049100       MOVE EJ-IFYLLD TO FAELT-TIHAEMT                                    
049200     ELSE                                                                 
049300     IF MID-TIHAEMT NUMERIC                                               
049400       MOVE RAETT     TO FAELT-TIHAEMT                                    
049500     ELSE                                                                 
049600       MOVE FEL       TO FAELT-TIHAEMT                                    
049700       MOVE NEJ       TO INDATA-RAETT-SW                                  
049800     END-IF                                                               
049900     END-IF                                                               
050000                                                                          
050100                                                                          
050200     MOVE 1 TO IX                                                         
050300                                                                          
050400     PERFORM UNTIL IX NOT < 7                                             
050500                                                                          
050600       IF MID-KDLBTYP (IX) = ALL '+'                                      
050700         MOVE EJ-IFYLLD TO FAELT-KDLBTYP (IX)                             
050800       ELSE                                                               
050900       IF MID-KDLBTYP (IX) NUMERIC                                        
051000         MOVE RAETT     TO FAELT-KDLBTYP (IX)                             
051100       ELSE                                                               
051200         MOVE FEL       TO FAELT-KDLBTYP (IX)                             
051300         MOVE NEJ       TO INDATA-RAETT-SW                                
051400       END-IF                                                             
051500       END-IF                                                             
051600                                                                          
051700       IF MID-IDLBBET (IX) = ALL '+'                                      
051800         MOVE EJ-IFYLLD TO FAELT-IDLBBET (IX)                             
051900       ELSE                                                               
052000         MOVE RAETT     TO FAELT-IDLBBET (IX)                             
052100       END-IF                                                             
052200                                                                          
052300       ADD 1 TO IX                                                        
052400                                                                          
052500     END-PERFORM                                                          
052600                                                                          
052700                                                                          
052800     MOVE 1 TO IX                                                         
052900                                                                          
053000     PERFORM UNTIL IX NOT < 4                                             
053100                                                                          
053200       IF MID-BETEXT-OEVR (IX) = ALL '+'                                  
053300         MOVE EJ-IFYLLD TO FAELT-BETEXT-OEVR (IX)                         
053400       ELSE                                                               
053500         MOVE RAETT     TO FAELT-BETEXT-OEVR (IX)                         
053600       END-IF                                                             
053700                                                                          
053800       ADD 1 TO IX                                                        
053900                                                                          
054000     END-PERFORM                                                          
054200                                                                          
054300                                                                          
054400     MOVE 1 TO IX                                                         
054500     MOVE 0 TO I                                                          
054600                                                                          
054700     PERFORM UNTIL IX NOT < 4                                             
054800                                                                          
054900       IF MID-FLTRDOK (IX) = ALL '+' OR SPACE                             
055000         MOVE EJ-IFYLLD TO FAELT-FLTRDOK (IX)                             
055100       ELSE                                                               
055200         IF MID-FLTRDOK (IX) = 'JA ' OR 'YES'                             
055300           MOVE JA      TO FAELT-FLTRDOK (IX)                             
055400           ADD 1 TO I                                                     
055500         ELSE                                                             
055600           IF MID-FLTRDOK (IX) = 'NEJ' OR 'NO ' OR 'NEE'                  
055700             MOVE NEJ   TO FAELT-FLTRDOK (IX)                             
055800           ELSE                                                           
055900             MOVE FEL   TO FAELT-FLTRDOK (IX)                             
056000             MOVE NEJ   TO INDATA-RAETT-SW                                
056100           END-IF                                                         
056200         END-IF                                                           
056300       END-IF                                                             
056400                                                                          
056500       ADD 1 TO IX                                                        
056600                                                                          
056700     END-PERFORM                                                          
056800                                                                          
056900     IF I < 1                                                             
057000       MOVE NEJ         TO INDATA-RAETT-SW                                
057100       MOVE 0           TO KDTRDOK-WS                                     
057200     ELSE                                                                 
057300       IF I > 1                                                           
057400         MOVE NEJ       TO INDATA-RAETT-SW                                
057500         MOVE 2         TO KDTRDOK-WS                                     
057600       ELSE                                                               
057700         MOVE 1         TO KDTRDOK-WS                                     
057800       END-IF                                                             
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 E-BEHANDLA-INDATA SECTION.                                               
058300     MOVE 'E-BEHANDL'      TO W-SEKTION                                   
058310                                                                          
058400     PERFORM EA-UPPDATERA-LASTDOK                                         
058500                                                                          
058600     IF MID-LB-GRP NOT = ALL '+' OR MID-OEVR-GRP NOT = ALL '+'            
058700       PERFORM EB-BILDA-TILLAEGG                                          
058800     END-IF                                                               
058900                                                                          
059500     .                                                                    
059600     EJECT                                                                
059700 EA-UPPDATERA-LASTDOK  SECTION.                                           
059800     MOVE 'EA-UPPDAT'      TO W-SEKTION                                   
059810                                                                          
059900     MOVE IDSHIPM-WS           TO W-IDSHIPM                               
060000     PERFORM IMS-GU-WDE101                                                
060001     MOVE SHIP-IDSHIPM         TO LAST-IDSHIPM                            
060002     MOVE SHIP-IDTRPTNR        TO LAST-IDTRPTNR                           
060003     MOVE SHIP-IDLBBET         TO LAST-IDLBBET-HUV                        
060004     MOVE SHIP-IDDC            TO LAST-IDDC                               
060005*                                                                         
060010     PERFORM IMS-GNP-WDE111                                               
060020     MOVE SGMT-IDDISTR         TO W-IDDISTR                               
060021     MOVE 1                    TO LAST-KVLDISTR                           
060030     PERFORM UNTIL SEGMENT-SAKNAS                                         
060031       IF W-IDDISTR NOT = SGMT-IDDISTR                                    
060032         MOVE 2                TO LAST-KVLDISTR                           
060033       END-IF                                                             
060040       PERFORM IMS-GNP-WDE111                                             
060050     END-PERFORM                                                          
060100                                                                          
060200     IF FAELT-TIAVGANG = RAETT                                            
060300       MOVE MID-TIAVGANG       TO LAST-TIAVGANG                           
060310                                  MOD-TIAVGANG                            
060400     END-IF                                                               
060500                                                                          
060600     IF FAELT-TIHAEMT  = RAETT                                            
060700       MOVE MID-TIHAEMT        TO LAST-TIHAEMT                            
060710                                  MOD-TIHAEMT                             
060800     END-IF                                                               
060900                                                                          
061000     IF FAELT-TINEDK   = RAETT                                            
061100       MOVE MID-TINEDK         TO LAST-TINEDK                             
061110                                  MOD-TINEDK                              
061200     END-IF                                                               
061300                                                                          
061400     IF FAELT-IDBOKN   = RAETT                                            
061500       MOVE MID-IDBOKN         TO LAST-IDBOKN                             
061510                                  MOD-IDBOKN                              
061600     END-IF                                                               
061700                                                                          
061800     IF FAELT-IDTRANSP-NAMN = RAETT                                       
061900       MOVE MID-IDTRANSP-NAMN  TO LAST-IDTRANSP-NAMN                      
061910                                  MOD-IDTRANSP-NAMN                       
062000     END-IF                                                               
062100                                                                          
062200     IF FAELT-BEROUTE  = RAETT                                            
062300       MOVE MID-BEROUTE        TO LAST-BEROUTE                            
062310                                  MOD-BEROUTE                             
062400     END-IF                                                               
062500                                                                          
062600     IF FAELT-BETEXT-HAEMT = RAETT                                        
062700       MOVE MID-BETEXT-HAEMT   TO LAST-BETEXT-HAEMT                       
062710                                  MOD-BETEXT-HAEMT                        
062800     END-IF                                                               
062900                                                                          
063000     IF FAELT-BETEXT-NEDK = RAETT                                         
063100       MOVE MID-BETEXT-NEDK    TO LAST-BETEXT-NEDK                        
063110                                  MOD-BETEXT-NEDK                         
063200     END-IF                                                               
063400                                                                          
063500     IF FAELT-FLTRDOK (1) = JA                                            
063600       MOVE 1                 TO LAST-KDTRDOK                             
063700     ELSE                                                                 
063800     IF FAELT-FLTRDOK (2) = JA                                            
063900       MOVE 2                 TO LAST-KDTRDOK                             
064000     ELSE                                                                 
064100       MOVE 3                 TO LAST-KDTRDOK                             
064200     END-IF                                                               
064300     END-IF                                                               
064400                                                                          
065100     .                                                                    
065200     EJECT                                                                
065300 EB-BILDA-TILLAEGG  SECTION.                                              
065400     MOVE 'EB-TILL'           TO W-SEKTION                                
065500                                                                          
065600     MOVE 1 TO IX                                                         
065700                                                                          
065800     PERFORM UNTIL IX NOT < 7                                             
065900       IF FAELT-KDLBTYP (IX) = RAETT                                      
066000         MOVE MID-KDLBTYP (IX)     TO LAST-KDLBTYP (IX)                   
066010                                      MOD-IDLBTYP  (IX)                   
066100       ELSE                                                               
066200         MOVE ZERO                 TO LAST-KDLBTYP (IX)                   
066210                                      MOD-IDLBTYP  (IX)                   
066300       END-IF                                                             
066400                                                                          
066500       IF FAELT-IDLBBET (IX) = RAETT                                      
066600         MOVE MID-IDLBBET (IX)     TO LAST-IDLBBET (IX)                   
066610                                      MOD-IDLBBET  (IX)                   
066700       ELSE                                                               
066800         MOVE SPACE                TO LAST-IDLBBET (IX)                   
066810                                      MOD-IDLBBET  (IX)                   
066900       END-IF                                                             
067000                                                                          
067100       IF IX < 4                                                          
067200         IF FAELT-BETEXT-OEVR (IX) = RAETT                                
067300           MOVE MID-BETEXT-OEVR (IX)                                      
067400                                 TO LAST-BETEXT-OEVR (IX)                 
067410                                    MOD-BETEXT-OEVR  (IX)                 
067500         ELSE                                                             
067600           MOVE SPACE            TO LAST-BETEXT-OEVR (IX)                 
067610                                    MOD-BETEXT-OEVR  (IX)                 
067700         END-IF                                                           
067800       END-IF                                                             
067900                                                                          
068000       ADD 1 TO IX                                                        
068100     END-PERFORM                                                          
068200                                                                          
068400     .                                                                    
068500     EJECT                                                                
073100 F-INDATA-FEL SECTION.                                                    
073200     MOVE 'F-INDAT'         TO W-SEKTION                                  
073210                                                                          
073300     MOVE IDSHIPM-WS        TO MOD-IDSHIPM                                
073400     INSPECT MOD-IDSHIPM REPLACING LEADING ZERO BY SPACE                  
073410     MOVE IDDC-WS           TO MOD-IDDC                                   
073420     INSPECT MOD-IDDC    REPLACING LEADING ZERO BY SPACE                  
073430     MOVE IDTRPTNR-WS       TO MOD-IDTRPTNR                               
073440     INSPECT MOD-IDTRPTNR REPLACING LEADING ZERO BY SPACE                 
073450     MOVE IDLBBET-WS        TO MOD-IDLBBET-HUV                            
073500                                                                          
073600     MOVE FEL-3 (SPRAAK-IX) TO MOD-TEMFSFEL                               
073700                                                                          
073800     PERFORM MFS-INDATA-FEL                                               
074100     .                                                                    
074110     SKIP2                                                                
074200 G-FELAKTIGT-ANROP SECTION.                                               
074300     MOVE 'G '         TO W-SEKTION                                       
074400     MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSINF                               
074410     MOVE JA                TO INDATA-RAETT-SW                            
074500                                                                          
074610     .                                                                    
074700     EJECT                                                                
074710 H-SKRIV-LISTA  SECTION.                                                  
074720     MOVE 'H-SKRIV'         TO W-SEKTION                                  
074721                                                                          
074726     MOVE PROGRAM-NAMN      TO TRPD-IDPGM                                 
074727     MOVE W-IDSHIPM         TO TRPD-IDSHIPM                               
074728     MOVE SGMT-IDDISTR      TO TRPD-IDDISTR                               
074729     MOVE SPACE             TO TRPD-IDPRTLST                              
074730     MOVE SPACE             TO TRPD-PFDEF-OVR                             
074731     CALL W476LAST   USING  TRPD-W476TRPD                                 
074732                            LAST-W4067410                                 
074733                            ALT1-PCB                                      
074734                            WDE1-PCB                                      
074735                            WDE6-PCB                                      
074736                            WDR1-PCB                                      
074737                            WDB9-PCB                                      
074738                            WDB5-PCB                                      
074739     .                                                                    
074740     EJECT                                                                
074750 K-STARTA-W40664   SECTION.                                               
074760                                                                          
074770     MOVE IDTRPTNR-WS         TO  MOD4664-MID-IDTRPTNR-IN                 
074780     MOVE IDLBBET-WS          TO  MOD4664-MID-IDLBBET-IN                  
074790     MOVE SPACE               TO  MOD4664-MID-FLFARLIG-IN                 
074791     MOVE W-IDDC              TO  MOD4664-MID-IDDC-IN                     
074792     IF NYCKLAR-FEL                                                       
074793       MOVE SPACE             TO  MOD4664-MID-IDTRPTNR-IN                 
074794                                  MOD4664-MID-IDLBBET-IN                  
074795                                  MOD4664-MID-IDDC-IN                     
074796     END-IF                                                               
074797     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX                      
074798                                   + 23 + 15                              
074799     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
074800     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
074801     MOVE 'W4T664  '          TO  P-TO-P-KDTRANS                          
074802     MOVE '4674'              TO  P-TO-P-IDTRANS                          
074803     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
074804     MOVE MOD4664-MID-W4I66401                                            
074805                              TO  P-TO-P-DATA                             
074806     PERFORM IMS-ISRT-ALT-MSG-4664                                        
074807     .                                                                    
074808     EJECT                                                                
074810******************* MFS SECTIONER *******************************         
074900                                                                          
075100 MFS-INDATA-FEL SECTION.                                                  
075200     MOVE 'MFS-INDATA-FEL'        TO W-SEKTION                            
075210                                                                          
075300     MOVE EGEN-MOD                TO MFS-IDMOD                            
075400     MOVE EGEN-MOD-LL             TO MSG-KVLL                             
075500     MOVE EGEN-TRANS              TO MOD-TRANS-NUMMER                     
075600                                                                          
075700     MOVE +1 TO FAELTTAB-IX                                               
075800     PERFORM UNTIL FAELTTAB-IX > FAELTTAB-IX-MAX                          
075900       IF FAELTTAB-FAELT (FAELTTAB-IX) = RAETT                            
076000         IF FAELTTAB-KDDATTYP (FAELTTAB-IX) = NUM                         
076100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ATTRIB (FAELTTAB-IX)          
076200         ELSE                                                             
076300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ATTRIB (FAELTTAB-IX)          
076400         END-IF                                                           
076500       ELSE                                                               
076600         IF FAELTTAB-FAELT (FAELTTAB-IX) = FEL                            
076700           IF FAELTTAB-KDDATTYP (FAELTTAB-IX) = NUM                       
076800             MOVE MFS-NUM-FAELT-FEL  TO MOD-ATTRIB (FAELTTAB-IX)          
076900           ELSE                                                           
077000             MOVE MFS-ALFA-FAELT-FEL TO MOD-ATTRIB (FAELTTAB-IX)          
077100           END-IF                                                         
077200         END-IF                                                           
077300       END-IF                                                             
077400       MOVE MFS-ROER-EJ-FAELT    TO MOD-FAELT (FAELTTAB-IX)               
077500       ADD +1 TO FAELTTAB-IX                                              
077600     END-PERFORM                                                          
077700     MOVE MFS-ROER-EJ-FAELT      TO MOD-FRAN-IDTRANS                      
077800     EJECT                                                                
077900                                                                          
078000     IF FAELT-BEROUTE = RAETT                                             
078100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEROUTE-ATTR                      
078200     ELSE                                                                 
078300     IF FAELT-BEROUTE = FEL                                               
078400       MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEROUTE-ATTR                      
078500     END-IF                                                               
078600     END-IF                                                               
078700                                                                          
078800     IF FAELT-IDBOKN = RAETT                                              
078900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDBOKN-ATTR                       
079000     ELSE                                                                 
079100     IF FAELT-IDBOKN = FEL                                                
079200       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDBOKN-ATTR                       
079300     END-IF                                                               
079400     END-IF                                                               
079500                                                                          
079600     IF FAELT-IDTRANSP-NAMN = RAETT                                       
079700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRANSP-NAMN-ATTR                
079800     ELSE                                                                 
079900     IF FAELT-IDTRANSP-NAMN = FEL                                         
080000       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDTRANSP-NAMN-ATTR                
080100     END-IF                                                               
080200     END-IF                                                               
080300                                                                          
080400     MOVE 1 TO IX                                                         
080500     PERFORM UNTIL IX NOT < 4                                             
080600       IF FAELT-FLTRDOK (IX) = FEL                                        
080700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTRDOK-ATTR (IX)               
080800       ELSE                                                               
080900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTRDOK-ATTR (IX)               
081000       END-IF                                                             
081100       ADD 1 TO IX                                                        
081200     END-PERFORM                                                          
081300                                                                          
081400     IF KDTRDOK-WS = 0 OR 2                                               
081500      MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTRDOK-ATTR (1)                     
081600                                 MOD-FLTRDOK-ATTR (2)                     
081700                                 MOD-FLTRDOK-ATTR (3)                     
081800     END-IF                                                               
081900                                                                          
082000     MOVE MFS-ROER-EJ-FAELT   TO MOD-BEROUTE                              
082100                                 MOD-IDBOKN                               
082200                                 MOD-IDTRANSP-NAMN                        
082300                                                                          
082400     MOVE MFS-RENSA-FAELT     TO MOD-TEMFSINF                             
082600     .                                                                    
082700     EJECT                                                                
085000 MFS-SVAR-START-MOD SECTION.                                              
085100     MOVE 'MFS-SVAR'             TO W-SEKTION                             
085110                                                                          
085200     MOVE EGEN-MOD               TO MFS-IDMOD                             
085300     MOVE EGEN-MOD-LL            TO MSG-KVLL                              
085900                                                                          
086000     MOVE +1                TO FAELTTAB-IX                                
086100     PERFORM UNTIL FAELTTAB-IX > FAELTTAB-IX-MAX                          
086200       MOVE MFS-ROER-EJ-FAELT TO MOD-FAELT (FAELTTAB-IX)                  
086300       ADD +1 TO FAELTTAB-IX                                              
086400     END-PERFORM                                                          
086410     .                                                                    
086500     SKIP3                                                                
091400*********************** IMS SECTIONER ************************            
091500                                                                          
091700 IMS-GET-MSG SECTION.                                                     
091800     MOVE '  QC'                TO GODK-STATUSKODER                       
091900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
092000     MOVE MSG-STATUS-CODE       TO STATUS-WS                              
092100     PERFORM IMS-STATUS-KONTROLL                                          
092110     .                                                                    
092200     SKIP3                                                                
092400 IMS-ISRT-MSG SECTION.                                                    
092500     IF ENGLISH-TEXT                                                      
092600         MOVE 'N' TO MFS-KDHUVOMR                                         
092700     END-IF                                                               
092800     MOVE LOW-VALUE             TO MSG-KDZ1 MSG-KDZ2                      
092900     MOVE SPACE                 TO GODK-STATUSKODER                       
093000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093100     MOVE MSG-STATUS-CODE       TO STATUS-WS                              
093200     PERFORM IMS-STATUS-KONTROLL                                          
093300     .                                                                    
093400     EJECT                                                                
093490 IMS-ISRT-ALT-MSG-4664 SECTION.                                           
093491                                                                          
093492     MOVE SPACE                TO GODK-STATUSKODER                        
093493     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW                           
093494     MOVE ALT2-STATUS-CODE     TO STATUS-WS                               
093495     PERFORM IMS-STATUS-KONTROLL                                          
093496     .                                                                    
093497     SKIP2                                                                
093500 IMS-GU-WDE101 SECTION.                                                   
093700                                                                          
093800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
093900          DELIMITED BY SIZE INTO SSA1                                     
094000     MOVE '    '               TO GODK-STATUSKODER                        
094100     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
094200     MOVE WDE1-STATUS-CODE     TO STATUS-WS                               
094300     PERFORM IMS-STATUS-KONTROLL                                          
094400     .                                                                    
094500     EJECT                                                                
094600 IMS-GNP-WDE111 SECTION.                                                  
094700                                                                          
094800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
094900          DELIMITED BY SIZE INTO SSA1                                     
094910     MOVE 'WDE111  '           TO SSA2                                    
095000     MOVE '  GE'               TO GODK-STATUSKODER                        
095100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
095200     MOVE WDE1-STATUS-CODE     TO STATUS-WS                               
095300     PERFORM IMS-STATUS-KONTROLL                                          
095400     .                                                                    
095500     EJECT                                                                
102200 IMS-STATUS-KONTROLL SECTION.                                             
102300     SET STATUS-IX TO 1                                                   
102400     SEARCH GODK-STATUS AT END CALL FELLOG                                
102500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
102600     END-SEARCH                                                           
102800     .                                                                    
102900     EJECT                                                                
