000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4070100.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   94/08/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MPP SOM ADMINISTRERAR ANSVARIG-TABELL                            
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WL4113 (WDR1) ANSVARIG ADM                 
001200*        PROGRAMMET UPPDATERAR WL4115 (WDR1) ANSVARIG RET                 
001300*        PROGRAMMET UPPDATERAR WL4117 (WDR1) ANSVARIG REM                 
001400*        PROGRAMMET UPPDATERAR WDP3                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T701                                              
001800*                     W4T701U                                             
001900*        MID:         W4I70101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O70101                                            
002210*                                                                         
002220*   2011-10-17  E'TRACKER 10143271 CHINA WAREHOUSE PROJECT-1              
002230*                                                                         
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900*    -- CHECKED BY WY2000                                                 
003000     SKIP3                                                                
003100 77  IDPGM                       PIC X(08)   VALUE 'W4070100'.            
003413 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  MAX-INDX                    PIC S9(3)   COMP-3 VALUE +9.             
004000 77  W-PRARTNTO                  PIC S9(7)V9(2) COMP-3                    
004100                                 VALUE 9999999.99.                        
004413 77  WS-IDLOPNR-PREV             PIC S9(3)   COMP-3 VALUE ZERO.           
004500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004600 01  WS-KDARBTYP-IDPERSON.                                                
004700     03  WS-SPAR-KDARBTYP        PIC X(3)    VALUE SPACE.                 
004800     03  WS-SPAR-IDPERSON        PIC 9(3)    VALUE ZERO.                  
004900                                                                          
005400 01  FILLER.                                                              
005500   03 WS-IDMAIL                  PIC X(60).                               
005600   03 FILLER    REDEFINES  WS-IDMAIL.                                     
005700     05 WS-IDMAIL30              PIC X(30).                               
005800     05 FILLER                   PIC X(30).                               
005900                                                                          
006600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006800*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
006900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
007000                                                                          
007100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007200 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
007300 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007400 77  WS-KDARBTYP                 PIC X(3)    VALUE SPACE.                 
007500                                                                          
007600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007700     88  INDATA-OK                           VALUE 'J'.                   
007800     88  INDATA-FEL                          VALUE 'N'.                   
007900                                                                          
008000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008100     88  NYCKLAR-OK                          VALUE 'J'.                   
008200     88  NYCKLAR-FEL                         VALUE 'N'.                   
008300                                                                          
008400 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
008500     88  UPDATE-SUCCESS                      VALUE 'J'.                   
008600     88  NO-UPDATE                           VALUE 'N'.                   
008700                                                                          
008807 77  DUPLICATE-SW                PIC X       VALUE 'N'.                   
008907     88  DUPLICATE                           VALUE 'J'.                   
009007     88  NO-DUPLICATE                        VALUE 'N'.                   
009107                                                                          
009207 77  SW-BORT                     PIC X       VALUE 'N'.                   
009307     88  BORTTAG-OK                          VALUE 'J'.                   
009407     88  INGET-BORTTAG                       VALUE 'N'.                   
009507                                                                          
009607 77  SW-AENDRA                   PIC X       VALUE 'N'.                   
009707     88  AENDRA-OK                           VALUE 'J'.                   
009807     88  AENDRA-INGET                        VALUE 'N'.                   
009907                                                                          
010007 77  SW-NYTT                     PIC X       VALUE 'N'.                   
010114     88  NYTT                                VALUE 'J'.                   
010217     88  EJ-NYTT                             VALUE 'N'.                   
010307                                                                          
010407 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010507     88  EGEN-MID                            VALUE '4701'.                
010607     88  GODK-MID                            VALUE '4701'.                
010707*    88  GODK-MID                            VALUE '4701' '4702'          
010807*                                                  '4703' '4704'          
010907*                                                  '4705' '4706'          
011007*                                                  '4707' '4708'          
011107*                                                  '4709'.                
011207     88  HELP-MID                            VALUE '0551'.                
011707     EJECT                                                                
011708                                                                          
011709*01  -COPY WWIDFTG                                                        
011710     EJECT                                                                
011807                                                                          
011907*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012007 01  GENERELLA-SUBPROGRAM.                                                
012107     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012207     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012307     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012407     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012507     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
012607     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012707     EJECT                                                                
012807*                                                                         
012907 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013007     SKIP3                                                                
013107*01 -COPY WMSGINIT                                                        
013207     EJECT                                                                
013307*                                                                         
013407 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
013507     SKIP3                                                                
013607*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013707*01 -COPY WMEDAREA                                                        
013807     EJECT                                                                
013907 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
014007     SKIP3                                                                
014107*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
014207*01 -COPY WDECAREA                                                        
014307     EJECT                                                                
014407 01  MESSAGE-CODES.                                                       
014507     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014607     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014707     03  ERR-SPECIFY-ONE-CHOICE  PIC X(3)    VALUE '004'.                 
014807     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014907     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015007     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015107     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
015207     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015307     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015407     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
015507     03  ERR-FEL-RADNR           PIC X(3)    VALUE '014'.                 
015607     03  ERR-FEL-KOD             PIC X(3)    VALUE '013'.                 
015707     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
015807     03  ERR-JUST-I-FORVAG-EJ-OK PIC X(3)    VALUE '008'.                 
015907     03  ERR-WRONG-CODE          PIC X(3)    VALUE '132'.                 
016007     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
016107     03  ERR-TOM-LESS-THAN-FOM   PIC X(3)    VALUE '240'.                 
016207     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
016307     03  ERR-DUPLICATE-RECORD    PIC X(3)    VALUE '245'.                 
016308     03  ERR-INTERVAL            PIC X(3)    VALUE '738'.                 
016407     EJECT                                                                
016507*01  -COPY W418OKOD    -PRE LKOD-                                         
016607     EJECT                                                                
016707*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016807*                                                                         
016907 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017007                                                                          
017107*01  MID -COPY W4I70101                                                   
017207     EJECT                                                                
017307 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017407                                                                          
017507*01  -COPY WMSGAREA                                                       
017607     EJECT                                                                
017707     03  MOD REDEFINES MSG-AREA.                                          
017807*      05  -COPY W4O70101                                                 
017907     EJECT                                                                
018007 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018107                                                                          
018207*01  -COPY WMFSAREA                                                       
018307     EJECT                                                                
018407*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018507*                                                                         
018607     EJECT                                                                
018707 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018807                                                                          
018907 01  NYCKLAR-TILL-DLI.                                                    
019007                                                                          
019107     03  W-KDARBTYP-X.                                                    
019207         05  W-KDARBTYP          PIC X(8)     VALUE SPACE.                
019307                                                                          
019407     03  W-IDPERSON-X.                                                    
019507         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
019607                                                                          
019707     03  W-WDGXKEY-ROT-4113-X.                                            
019807         05  FILLER              PIC X(4)     VALUE '4113'.               
019907         05  W-IDFTG-4113        PIC 9(2)     VALUE ZERO.                 
020007         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
020107                                                                          
020207     03  W-KEY4114-MIN-X.                                                 
020307         05  W-KDANMORS-4114-MIN PIC X(2)    VALUE SPACE.                 
020407         05  W-IDLOPNR-4114-MIN  PIC S9(3)   COMP-3 VALUE ZERO.           
020507                                                                          
020607     03  W-KEY4114-MAX-X.                                                 
020707         05  W-KDANMORS-4114-MAX PIC X(2)    VALUE SPACE.                 
020807         05  W-IDLOPNR-4114-MAX PIC S9(3)   COMP-3 VALUE ZERO.            
020907                                                                          
021007     03  W-KEY4114-X.                                                     
021107         05  W-KDANMORS-4114     PIC X(2)    VALUE SPACE.                 
021207         05  W-IDLOPNR-4114      PIC S9(3)   COMP-3 VALUE ZERO.           
021307                                                                          
021407     03  W-WDGXKEY-ROT-4115-X.                                            
021507         05  FILLER              PIC X(4)     VALUE '4115'.               
021607         05  W-IDFTG-4115        PIC 9(2)     VALUE ZERO.                 
021707         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
021807                                                                          
021907     03  W-KEY4116-MIN-X.                                                 
022007         05  W-KDANMORS-4116-MIN PIC X(2)    VALUE SPACE.                 
022107         05  W-IDLOPNR-4116-MIN  PIC S9(3)   COMP-3 VALUE ZERO.           
022207                                                                          
022307     03  W-KEY4116-MAX-X.                                                 
022407         05  W-KDANMORS-4116-MAX PIC X(2)    VALUE SPACE.                 
022507         05  W-IDLOPNR-4116-MAX PIC S9(3)   COMP-3 VALUE ZERO.            
022607                                                                          
022707     03  W-KEY4116-X.                                                     
022807         05  W-KDANMORS-4116     PIC X(2)    VALUE SPACE.                 
022907         05  W-IDLOPNR-4116      PIC S9(3)   COMP-3 VALUE ZERO.           
023007                                                                          
023107     03  W-WDGXKEY-ROT-4117-X.                                            
023207         05  FILLER              PIC X(4)     VALUE '4117'.               
023307         05  W-IDFTG-4117        PIC 9(2)     VALUE ZERO.                 
023407         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
023507                                                                          
023607     03  W-KEY4118-MIN-X.                                                 
023707         05  W-KDANMORS-4118-MIN PIC X(2)    VALUE SPACE.                 
023807         05  W-IDDC-4118-MIN     PIC X(2)    VALUE SPACE.                 
023907         05  W-IDLOPNR-4118-MIN  PIC S9(3)   COMP-3 VALUE ZERO.           
024007                                                                          
024107     03  W-KEY4118-MAX-X.                                                 
024207         05  W-KDANMORS-4118-MAX PIC X(2)    VALUE SPACE.                 
024307         05  W-IDDC-4118-MAX     PIC X(2)    VALUE SPACE.                 
024409         05  W-IDLOPNR-4118-MAX PIC S9(3)    COMP-3 VALUE ZERO.           
024507                                                                          
024609     03  W-KEY4118-X.                                                     
024707         05  W-KDANMORS-4118     PIC X(2)    VALUE SPACE.                 
024807         05  W-IDDC-4118         PIC X(2)    VALUE SPACE.                 
024907         05  W-IDLOPNR-4118      PIC S9(3)   COMP-3 VALUE ZERO.           
025007                                                                          
025907     03  W-IDDISTRF-X.                                                    
026007         05  W-IDDISTRF          PIC S9(5)   COMP-3.                      
026107                                                                          
026207     03  W-IDDISTRT-X.                                                    
026307         05  W-IDDISTRT          PIC S9(5)   COMP-3.                      
026407                                                                          
026507     03  W-IDKUNDNF-X.                                                    
026607         05  W-IDKUNDNF          PIC S9(7)   COMP-3.                      
026707                                                                          
026807     03  W-IDKUNDNT-X.                                                    
026907         05  W-IDKUNDNT          PIC S9(7)   COMP-3.                      
027007                                                                          
027107     03  W-ADLAGOMR-X.                                                    
027207         05  W-ADLAGOMR          PIC S9(3)   COMP-3.                      
027307                                                                          
027407     03  W-KDORDKL-X.                                                     
027507         05  W-KDORDKL           PIC S9      COMP-3.                      
027607                                                                          
028307     03  W-IDDISTR-FOM-X.                                                 
028407         05  W-IDDISTR-FOM       PIC S9(5)   COMP-3.                      
028507                                                                          
028607     03  W-IDDISTR-TOM-X.                                                 
028707         05  W-IDDISTR-TOM       PIC S9(5)   COMP-3.                      
029907                                                                          
030007     03  W-IDDC-B6-X.                                                     
030107         05 W-IDDC-B6                  PIC X(2).                          
030207                                                                          
030307     03  W-IDDC-MIN-X.                                                    
030407         05 W-IDDC-MIN                 PIC X(2) VALUE LOW-VALUE.          
030507                                                                          
030607     03  W-IDDC-MAX-X.                                                    
030707         05 W-IDDC-MAX                 PIC X(2) VALUE HIGH-VALUE.         
030807                                                                          
030907*    --- STATUS-KOD FRÅN IMS                                              
031007 01  STATUS-WS                   PIC XX.                                  
031107     88  SEGMENT-FINNS                       VALUE '  '.                  
031207     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031307     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031407                                                                          
031507 01  GODK-STATUSKODER.                                                    
031607     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031707                                                                          
031807 01  SSA1                        PIC X(255).                              
031907 01  SSA2                        PIC X(64).                               
032007     EJECT                                                                
032107*    --- IMS FUNKTIONSKODER                                               
032207*01  -COPY W0003                                                          
032307     EJECT                                                                
032407*    ---  DLI INPUT-OUTPUT AREA                                           
032507 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4113'.            
032607                                                                          
032707 01  DLI-IO-AREA-4113.                                                    
032807*        05  -COPY WDGX4113                                               
032907     EJECT                                                                
033007*                                                                         
033107 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4114'.            
033207                                                                          
033307 01  DLI-IO-AREA-4114.                                                    
033407*        05  -COPY WDGX4114                                               
033507     EJECT                                                                
033607*                                                                         
033707 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4115'.            
033807                                                                          
033907 01  DLI-IO-AREA-4115.                                                    
034007*        05  -COPY WDGX4115                                               
034107     EJECT                                                                
034207*                                                                         
034307 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4116'.            
034407                                                                          
034507 01  DLI-IO-AREA-4116.                                                    
034607*        05  -COPY WDGX4116                                               
034707     EJECT                                                                
034807*                                                                         
034907 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4117'.            
035007                                                                          
035107 01  DLI-IO-AREA-4117.                                                    
035207*        05  -COPY WDGX4117                                               
035307     EJECT                                                                
035407*                                                                         
035507 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4118'.            
035607                                                                          
035707 01  DLI-IO-AREA-4118.                                                    
035807*        05  -COPY WDGX4118                                               
035907     EJECT                                                                
036007*                                                                         
036107 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-P311'.            
036207                                                                          
036307 01  DLI-IO-AREA-P311.                                                    
036407*        05  -COPY WDP311                                                 
036507     EJECT                                                                
037407 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
037507 01   DLI-IO-AREA-B601.                                                   
037607*     03  -COPY WDB601                                                    
037707                                                                          
037807 LINKAGE SECTION.                                                         
037907                                                                          
038007*01  -COPY W0009   -PRE MSG-                                              
038107     EJECT                                                                
038207*01  -COPY W0008  -PRE 4113-                                              
038307     05  FILLER                  PIC X.                                   
038407     EJECT                                                                
038507*01  -COPY W0008  -PRE WDP3-                                              
038607     05  FILLER                  PIC X.                                   
038707     EJECT                                                                
038807*01  -COPY W0008  -PRE 4115-                                              
038907     05  FILLER                  PIC X.                                   
039007     EJECT                                                                
039107*01  -COPY W0008  -PRE 4117-                                              
039207     05  FILLER                  PIC X.                                   
039307     EJECT                                                                
039407*01  -COPY W0008  -PRE USEA-                                              
039507     05  FILLER                  PIC X.                                   
039607     EJECT                                                                
040307*01  -COPY W0008  -PRE WDB6-                                              
040407     05  FILLER                  PIC X.                                   
040507     EJECT                                                                
040607 PROCEDURE DIVISION  USING MSG-PCB 4113-PCB WDP3-PCB                      
040707                                   4115-PCB 4117-PCB USEA-PCB             
040820                                   WDB6-PCB.                              
040907     ENTRY 'DLITCBL' USING MSG-PCB 4113-PCB WDP3-PCB                      
041007                                   4115-PCB 4117-PCB USEA-PCB             
041120                                   WDB6-PCB.                              
041207                                                                          
041307     PERFORM IMS-GET-MSG                                                  
041407     IF SEGMENT-FINNS                                                     
041507       PERFORM A-INIT                                                     
041607       PERFORM B-KOLLA-NYCKLAR                                            
041707       IF NYCKLAR-OK                                                      
041807         IF MFS-UPDATE                                                    
041907           PERFORM G-KOLLA-INPUT                                          
042007           IF INDATA-OK                                                   
042107             PERFORM H-UPPDATERA                                          
042207           END-IF                                                         
042307         ELSE                                                             
042407           IF MFS-FIRST                                                   
042507             PERFORM C-FOERSTA-SIDA                                       
042607           ELSE                                                           
042707             IF MFS-NEXT                                                  
042807               PERFORM D-NAESTA-SIDA                                      
042907             ELSE                                                         
043007               PERFORM E-SAMMA-SIDA                                       
043107             END-IF                                                       
043207           END-IF                                                         
043307         END-IF                                                           
043407         IF INDATA-OK                                                     
043507             PERFORM F-LAES-VISA-INFO                                     
043607         END-IF                                                           
043707       END-IF                                                             
043807       MOVE MAX-MOD-LAENGD            TO MSG-KVLL                         
043907       PERFORM IMS-INSERT-MSG                                             
044007     END-IF                                                               
044107                                                                          
044207     MOVE ZERO TO RETURN-CODE                                             
044307     GOBACK                                                               
044407     .                                                                    
044507     EJECT                                                                
044607 A-INIT SECTION.                                                          
044707                                                                          
044807     IF MSG-DUBBLA-TRANSKODER                                             
044907       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
045007                                      TO MID-W4I70101                     
045107       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
045207       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
045307     ELSE                                                                 
045407       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
045507                                      TO MID-W4I70101                     
045607       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
045707       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
045807     END-IF                                                               
045907                                                                          
046007     MOVE SPACE                       TO MED-IDMFSINF                     
046107     MOVE SPACE                       TO MED-IDMFSFEL                     
046207                                                                          
046307     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
046407     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
046507     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
046607                                                                          
046707     MOVE LOW-VALUE                   TO MSG-AREA                         
046807     MOVE 'W4O70101'                  TO MFS-IDMOD                        
046907     MOVE '4701'                      TO MOD-IDTRANS                      
047007     MOVE MFS-RENSA-FAELT             TO MOD-TEMFSFEL                     
047107                                         MOD-TEMFSINF                     
047207                                                                          
047307     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O70101 + 4                  
047407                                                                          
047507     IF EGEN-MID OR HELP-MID                                              
047607       CONTINUE                                                           
047707     ELSE                                                                 
047807       MOVE SPACE                     TO MFS-KDTRTYP                      
047907       MOVE '7'                       TO MFS-IDPFK                        
048007     END-IF                                                               
048507     .                                                                    
048607     EJECT                                                                
048707 B-KOLLA-NYCKLAR SECTION.                                                 
048807                                                                          
048907     MOVE JA TO NYCKLAR-SW                                                
049007                                                                          
049107*    -- KONTROLL AV KDANMORS                                              
049207     MOVE MFS-RENSA-FAELT             TO MOD-KDANMORS-IN                  
049307                                         MOD-IDDISTR-IN                   
049407                                         MOD-KDARBTYP-IN                  
049507                                         MOD-IDDC-IN                      
049607                                                                          
049707*                                                                         
049807     MOVE LOW-VALUE                   TO W-KEY4114-MIN-X                  
049907                                         W-KEY4116-MIN-X                  
050007                                         W-KEY4118-MIN-X                  
050107                                         W-KEY4114-X                      
050207                                         W-KEY4116-X                      
050307                                         W-KEY4118-X                      
050407                                         W-IDDISTR-TOM-X                  
050507     MOVE HIGH-VALUE                  TO W-KEY4114-MAX-X                  
050607                                         W-KEY4116-MAX-X                  
050707                                         W-KEY4118-MAX-X                  
050807                                         W-IDDISTR-FOM-X                  
050907                                                                          
051007     PERFORM BA-KOLLA-KDARBTYP                                            
051107                                                                          
051207     IF MFS-UPDATE                                                        
051307         MOVE MID-KDANMORS-UT         TO WS-KDANMORS                      
051407     ELSE                                                                 
051507         PERFORM BB-KOLLA-KDANMORS                                        
051607     END-IF                                                               
051707     PERFORM BE-KOLLA-IDDC                                                
051807     PERFORM BC-KOLLA-IDDISTR                                             
051907     PERFORM BD-HAEMTA-USERINFO                                           
052007                                                                          
052107     IF MSGI-IDLAND-SPR = 'GB'                                            
052207       MOVE 'GB '                     TO MED-IDSKYLT                      
052307     ELSE                                                                 
052407       MOVE 'S  '                     TO MED-IDSKYLT                      
052507     END-IF                                                               
052607                                                                          
052707     IF GODK-MID OR NYCKLAR-OK                                            
052807       MOVE WS-KDARBTYP               TO MOD-KDARBTYP-UT                  
052907       MOVE WS-KDANMORS               TO MOD-KDANMORS-UT                  
053007       MOVE WS-IDDISTR                TO MOD-IDDISTR-UT                   
053107       MOVE WS-IDDC                   TO MOD-IDDC-UT                      
053207       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
053307     ELSE                                                                 
053407       MOVE MFS-RENSA-FAELT           TO MOD-KDARBTYP-UT                  
053507                                         MOD-KDANMORS-UT                  
053607                                         MOD-IDDISTR-UT                   
053707                                         MOD-IDDC-UT                      
053807     END-IF                                                               
053907                                                                          
054007     IF NYCKLAR-FEL                                                       
054107       MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                     
054207       CALL WMEDKONV USING MED-WMEDAREA                                   
054307       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
054407       PERFORM MFS-RENSA-FAELT-IN                                         
054507       PERFORM MFS-RENSA-FAELT-UT                                         
054607     END-IF                                                               
054707     .                                                                    
054807     EJECT                                                                
054907 BA-KOLLA-KDARBTYP              SECTION.                                  
055007                                                                          
055107     IF MID-KDARBTYP-IN NOT = ALL '+' AND EGEN-MID                        
055207       MOVE MID-KDARBTYP-IN           TO WS-KDARBTYP                      
055307       MOVE '7'                       TO MFS-IDPFK                        
055407       MOVE SPACE                     TO MFS-KDTRTYP                      
055507     ELSE                                                                 
055607       MOVE MID-KDARBTYP-UT           TO WS-KDARBTYP                      
055707     END-IF                                                               
055807                                                                          
055907     IF WS-KDARBTYP = 'ADM' OR 'RET' OR 'REM'                             
056007       CONTINUE                                                           
056107     ELSE                                                                 
056207       MOVE NEJ                       TO NYCKLAR-SW                       
056307     END-IF                                                               
056407     .                                                                    
056507     EJECT                                                                
056607 BB-KOLLA-KDANMORS              SECTION.                                  
056707                                                                          
056807     IF MID-KDANMORS-IN NOT = ALL '+' AND EGEN-MID                        
056907       MOVE MID-KDANMORS-IN           TO WS-KDANMORS                      
057007       MOVE '7'                       TO MFS-IDPFK                        
057107       MOVE SPACE                     TO MFS-KDTRTYP                      
057207     ELSE                                                                 
057307       MOVE MID-KDANMORS-UT           TO WS-KDANMORS                      
057407     END-IF                                                               
057507                                                                          
057607     IF WS-KDANMORS = '  ' OR 'ÖÖ'                                        
057707        CONTINUE                                                          
057807     ELSE                                                                 
057907        EVALUATE WS-KDARBTYP                                              
058007           WHEN 'ADM'                                                     
058107              MOVE WS-KDANMORS        TO W-KDANMORS-4114-MIN              
058207                                         W-KDANMORS-4114-MAX              
058307           WHEN 'RET'                                                     
058407              MOVE WS-KDANMORS        TO W-KDANMORS-4116-MIN              
058507                                         W-KDANMORS-4116-MAX              
058607           WHEN 'REM'                                                     
058707              MOVE WS-KDANMORS        TO W-KDANMORS-4118-MIN              
058807                                         W-KDANMORS-4118-MAX              
058907        END-EVALUATE                                                      
059007     END-IF                                                               
059107     .                                                                    
059207     EJECT                                                                
059307 BE-KOLLA-IDDC                  SECTION.                                  
059407                                                                          
059507     IF MID-IDDC-IN NOT = ALL '+' AND EGEN-MID                            
059607       MOVE MID-IDDC-IN               TO WS-IDDC                          
059707       MOVE '7'                       TO MFS-IDPFK                        
059807       MOVE SPACE                     TO MFS-KDTRTYP                      
059907     ELSE                                                                 
060007       MOVE MID-IDDC-UT               TO WS-IDDC                          
060107     END-IF                                                               
060207                                                                          
060307     EVALUATE WS-KDARBTYP                                                 
060407       WHEN 'REM'                                                         
061000         IF WS-IDDC > SPACE                                               
061100           MOVE WS-IDDC          TO W-IDDC-MIN                            
061200                                    W-IDDC-MAX                            
061300           IF W-KDANMORS-4118-MIN NUMERIC OR                              
061400              W-KDANMORS-4118-MIN = 'XX'                                  
061500             MOVE WS-IDDC          TO W-IDDC-4118-MIN                     
061600                                      W-IDDC-4118-MAX                     
061700           END-IF                                                         
061800         END-IF                                                           
061900     END-EVALUATE                                                         
062000     .                                                                    
062100     EJECT                                                                
062200 BC-KOLLA-IDDISTR               SECTION.                                  
062300                                                                          
062400     IF MID-IDDISTR-IN NOT = ALL '+'  AND EGEN-MID                        
062500       MOVE MID-IDDISTR-IN            TO WS-IDDISTR                       
062600       MOVE '7'                       TO MFS-IDPFK                        
062700       MOVE SPACE                     TO MFS-KDTRTYP                      
062800     ELSE                                                                 
062900       MOVE MID-IDDISTR-UT            TO WS-IDDISTR                       
063000     END-IF                                                               
063100                                                                          
063200     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
063300*                                                                         
063400     IF WS-IDDISTR NUMERIC                                                
063500       IF WS-IDDISTR > ZERO                                               
063600          MOVE WS-IDDISTR             TO W-IDDISTR-FOM                    
063700                                         W-IDDISTR-TOM                    
064000       END-IF                                                             
064100     ELSE                                                                 
064200       MOVE NEJ                       TO NYCKLAR-SW                       
064300     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600 BD-HAEMTA-USERINFO SECTION.                                              
064700                                                                          
064800     MOVE ALL '+'                     TO MSGI-WMSGINIT                    
064900     MOVE '001'                       TO MSGI-KDCALL                      
065000     MOVE MSG-SIGNON-USERID           TO MSGI-IDUSER                      
065100     MOVE '4701'                      TO MSGI-IDTRANS                     
065200     MOVE MSG-LTERM-NAME              TO MSGI-IDLTERM-USER                
065300                                                                          
065400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065500                                                                          
065600     MOVE MSGI-IDFTG                  TO W-IDFTG-4113                     
065700                                         W-IDFTG-4115                     
065800                                         W-IDFTG-4117                     
065900                                                                          
066000                                                                          
066100     .                                                                    
066200     EJECT                                                                
066300 C-FOERSTA-SIDA SECTION.                                                  
066400                                                                          
066500     MOVE INF-FIRST-PAGE              TO MED-IDMFSINF                     
066600     CALL WMEDKONV USING MED-WMEDAREA                                     
066700     MOVE MED-MFSINF                  TO MOD-TEMFSFEL                     
066800                                                                          
066900*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
067000     MOVE ZERO                        TO MOD-IDRADNR-ENTER                
067100                                         MOD-IDRADNR-NEXT                 
067200     MOVE SPACE                       TO MOD-KDANMORS-ENTER               
067300                                         MOD-KDARBTYP-ENTER               
067400                                         MOD-KDARBTYP-NEXT                
067500                                         MOD-IDDC-ENTER                   
067600                                         MOD-IDDC-NEXT                    
067700     MOVE 'ÖÖ'                        TO MOD-KDANMORS-NEXT                
067800     PERFORM MFS-RENSA-FAELT-IN                                           
067900                                                                          
068000     IF WS-KDARBTYP = 'RET' OR 'ADM'                                      
068100       MOVE MFS-CLOSE-FIELD    TO MOD-KDORDKL-UPPD-ATTR                   
068200                                  MOD-ADLAGOMR-UPPD-ATTR                  
068300     END-IF                                                               
068400                                                                          
068500     .                                                                    
068600     EJECT                                                                
068700 D-NAESTA-SIDA SECTION.                                                   
068800                                                                          
068900     MOVE HIGH-VALUE                  TO W-KEY4114-MAX-X                  
069000                                         W-KEY4116-MAX-X                  
069100                                         W-KEY4118-MAX-X                  
069200                                                                          
069300     MOVE MID-KDARBTYP-NEXT           TO WS-KDARBTYP                      
069400     EVALUATE WS-KDARBTYP                                                 
069500        WHEN 'ADM'                                                        
069600           IF MID-KDANMORS-NEXT = 'ÖÖ'                                    
069700              MOVE HIGH-VALUES        TO W-KDANMORS-4114-MIN              
069800           ELSE                                                           
069900              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4114-MIN              
070000           END-IF                                                         
070100           IF WS-KDANMORS = SPACE                                         
070200              CONTINUE                                                    
070300           ELSE                                                           
070400              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4114-MAX              
070500           END-IF                                                         
070600           MOVE MID-IDRADNR-NEXT      TO W-IDLOPNR-4114-MIN               
070700        WHEN 'RET'                                                        
070800           IF MID-KDANMORS-NEXT = 'ÖÖ'                                    
070900              MOVE HIGH-VALUES        TO W-KDANMORS-4116-MIN              
071000           ELSE                                                           
071100              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4116-MIN              
071200           END-IF                                                         
071300           IF WS-KDANMORS = SPACE                                         
071400              CONTINUE                                                    
071500           ELSE                                                           
071600              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4116-MAX              
071700           END-IF                                                         
071800           MOVE MID-IDRADNR-NEXT      TO W-IDLOPNR-4116-MIN               
071900        WHEN 'REM'                                                        
072000           IF MID-KDANMORS-NEXT = 'ÖÖ'                                    
072100              MOVE LOW-VALUES        TO W-KDANMORS-4118-MIN               
072200           ELSE                                                           
072300              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4118-MIN              
072400           END-IF                                                         
072500           IF WS-KDANMORS = SPACE                                         
072600              CONTINUE                                                    
072700           ELSE                                                           
072800              MOVE MID-KDANMORS-NEXT  TO W-KDANMORS-4118-MAX              
072900           END-IF                                                         
073000           MOVE MID-IDDC-NEXT         TO W-IDDC-4118-MIN                  
073100           MOVE MID-IDRADNR-NEXT      TO W-IDLOPNR-4118-MIN               
073200     END-EVALUATE                                                         
073300                                                                          
073400     PERFORM MFS-RENSA-FAELT-IN                                           
073500     .                                                                    
073600     EJECT                                                                
073700 E-SAMMA-SIDA SECTION.                                                    
073800                                                                          
073900     MOVE MID-KDARBTYP-ENTER          TO WS-KDARBTYP                      
074000     IF EGEN-MID OR HELP-MID                                              
074100        EVALUATE WS-KDARBTYP                                              
074200           WHEN 'ADM'                                                     
074300              IF MID-KDANMORS-ENTER = SPACE                               
074400                  MOVE LOW-VALUE      TO W-KDANMORS-4114-MIN              
074500                  MOVE HIGH-VALUE     TO W-KDANMORS-4114-MAX              
074600              ELSE                                                        
074700                  MOVE MID-KDANMORS-ENTER                                 
074800                                      TO W-KDANMORS-4114-MIN              
074900              END-IF                                                      
075000                                                                          
075100              IF MID-IDRADNR-ENTER NUMERIC                                
075200                MOVE MID-IDRADNR-ENTER                                    
075300                                      TO W-IDLOPNR-4114-MIN               
075400              ELSE                                                        
075500                MOVE ZERO             TO W-IDLOPNR-4114-MIN               
075600              END-IF                                                      
075700           WHEN 'RET'                                                     
075800              IF MID-KDANMORS-ENTER = SPACE                               
075900                  MOVE LOW-VALUE      TO W-KDANMORS-4116-MIN              
076000                  MOVE HIGH-VALUE     TO W-KDANMORS-4116-MAX              
076100              ELSE                                                        
076200                  MOVE MID-KDANMORS-ENTER                                 
076300                                      TO W-KDANMORS-4116-MIN              
076400              END-IF                                                      
076500                                                                          
076600              IF MID-IDRADNR-ENTER NUMERIC                                
076700                MOVE MID-IDRADNR-ENTER                                    
076800                                      TO W-IDLOPNR-4116-MIN               
076900              ELSE                                                        
077000                MOVE ZERO             TO W-IDLOPNR-4116-MIN               
077100              END-IF                                                      
077200           WHEN 'REM'                                                     
077300              IF MID-KDANMORS-ENTER = SPACE                               
077400                  MOVE LOW-VALUE      TO W-KDANMORS-4118-MIN              
077500                  MOVE HIGH-VALUE     TO W-KDANMORS-4118-MAX              
077600              ELSE                                                        
077700                  MOVE MID-KDANMORS-ENTER                                 
077800                                      TO W-KDANMORS-4118-MIN              
077900              END-IF                                                      
078000                                                                          
078100              MOVE MID-IDDC-ENTER     TO W-IDDC-4118-MIN                  
078200                                                                          
078300              IF MID-IDRADNR-ENTER NUMERIC                                
078400                MOVE MID-IDRADNR-ENTER                                    
078500                                      TO W-IDLOPNR-4118-MIN               
078600              ELSE                                                        
078700                MOVE ZERO             TO W-IDLOPNR-4118-MIN               
078800              END-IF                                                      
078900        END-EVALUATE                                                      
079000                                                                          
079100        IF MID-INPUT = ALL '+'                                            
079200          PERFORM MFS-RENSA-FAELT-IN                                      
079300        ELSE                                                              
079400          MOVE INF-PRESS-PF11         TO MED-IDMFSINF                     
079500          CALL WMEDKONV USING MED-WMEDAREA                                
079600          MOVE MED-MFSINF             TO MOD-TEMFSFEL                     
079700          PERFORM EA-MID-INDATA-TILL-MOD                                  
079800        END-IF                                                            
079900     ELSE                                                                 
080000       PERFORM MFS-RENSA-FAELT-IN                                         
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 EA-MID-INDATA-TILL-MOD SECTION.                                          
080500                                                                          
080600     IF MID-IDRADNR-UPPD NOT = ALL '+'                                    
080700       MOVE MID-IDRADNR-UPPD          TO MOD-IDRADNR-UPPD                 
080800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDRADNR-UPPD-ATTR            
080900     ELSE                                                                 
081000       MOVE MFS-RENSA-FAELT           TO MOD-IDRADNR-UPPD                 
081100     END-IF                                                               
081200                                                                          
081300     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
081400       MOVE MID-KDANMORS-UPPD         TO MOD-KDANMORS-UPPD                
081500       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDANMORS-UPPD-ATTR           
081600     ELSE                                                                 
081700       MOVE HIGH-VALUE                TO MID-KDANMORS-UPPD                
081800                                         MOD-KDANMORS-UPPD                
081900       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDANMORS-UPPD-ATTR           
082000     END-IF                                                               
082100                                                                          
082200     IF MID-IDDC-UPPD NOT = ALL '+'                                       
082300       MOVE MID-IDDC-UPPD             TO MOD-IDDC-UPPD                    
082400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDDC-UPPD-ATTR               
082500     ELSE                                                                 
082600       MOVE MFS-RENSA-FAELT           TO MOD-IDDC-UPPD                    
082700     END-IF                                                               
082800                                                                          
082900     IF WS-KDARBTYP = 'REM'                                               
083000       IF MID-KDORDKL-UPPD NOT = ALL '+'                                  
083100         MOVE MID-KDORDKL-UPPD          TO MOD-KDORDKL-UPPD               
083200         MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDORDKL-UPPD-ATTR          
083300       ELSE                                                               
083400         MOVE MFS-RENSA-FAELT           TO MOD-KDORDKL-UPPD               
083500       END-IF                                                             
083600                                                                          
083700       IF MID-ADLAGOMR-UPPD NOT = ALL '+'                                 
083800         MOVE MID-ADLAGOMR-UPPD         TO MOD-ADLAGOMR-UPPD              
083900         MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-ADLAGOMR-UPPD-ATTR         
084000       ELSE                                                               
084100         MOVE MFS-RENSA-FAELT           TO MOD-ADLAGOMR-UPPD              
084200       END-IF                                                             
084300     END-IF                                                               
084400                                                                          
084500     IF MID-IDDISTR-FOM-UPPD NOT = ALL '+'                                
084600       MOVE MID-IDDISTR-FOM-UPPD      TO MOD-IDDISTR-FOM-UPPD             
084700       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDDISTR-FOM-UPPD-ATTR        
084800     ELSE                                                                 
084900       MOVE MFS-RENSA-FAELT           TO MOD-IDDISTR-FOM-UPPD             
085000     END-IF                                                               
085100                                                                          
085200     IF MID-IDDISTR-TOM-UPPD NOT = ALL '+'                                
085300       MOVE MID-IDDISTR-TOM-UPPD      TO MOD-IDDISTR-TOM-UPPD             
085400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDDISTR-TOM-UPPD-ATTR        
085500     ELSE                                                                 
085600       MOVE MFS-RENSA-FAELT           TO MOD-IDDISTR-TOM-UPPD             
085700     END-IF                                                               
085800                                                                          
085900     IF MID-IDKUNDNR-FOM-UPPD NOT = ALL '+'                               
086000       MOVE MID-IDKUNDNR-FOM-UPPD     TO MOD-IDKUNDNR-FOM-UPPD            
086100       MOVE MFS-ADD-LAES-IN-FAELT     TO                                  
086200                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
086300     ELSE                                                                 
086400       MOVE MFS-RENSA-FAELT           TO MOD-IDKUNDNR-FOM-UPPD            
086500     END-IF                                                               
086600                                                                          
086700     IF MID-IDKUNDNR-TOM-UPPD NOT = ALL '+'                               
086800       MOVE MID-IDKUNDNR-TOM-UPPD     TO MOD-IDKUNDNR-TOM-UPPD            
086900       MOVE MFS-ADD-LAES-IN-FAELT     TO                                  
087000                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
087100     ELSE                                                                 
087200       MOVE MFS-RENSA-FAELT           TO MOD-IDKUNDNR-TOM-UPPD            
087300     END-IF                                                               
087400                                                                          
087500     MOVE MID-KDARB-IDPERS-UPPD       TO WS-KDARBTYP-IDPERSON             
087600                                                                          
087700     IF WS-SPAR-KDARBTYP NOT = ALL '+'                                    
087800     AND WS-SPAR-IDPERSON NOT = ALL '+'                                   
087900       MOVE WS-KDARBTYP-IDPERSON      TO MOD-KDARB-IDPERS-UPD             
088000       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDARB-IDPERS-UPD-ATTR        
088100     ELSE                                                                 
088200       MOVE MFS-RENSA-FAELT           TO MOD-KDARB-IDPERS-UPD-ATTR        
088300     END-IF                                                               
088400                                                                          
088500     IF MID-FLBORT-UPPD NOT = '+'                                         
088600       MOVE MID-FLBORT-UPPD           TO MOD-FLBORT-UPPD                  
088700       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLBORT-UPPD-ATTR             
088800     ELSE                                                                 
088900       MOVE MFS-RENSA-FAELT           TO MOD-FLBORT-UPPD                  
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 F-LAES-VISA-INFO SECTION.                                                
089400                                                                          
089500     EVALUATE WS-KDARBTYP                                                 
089600        WHEN 'ADM'                                                        
089700           IF MFS-UPDATE                                                  
089800              MOVE W-KDANMORS-4114    TO W-KDANMORS-4114-MIN              
089900              MOVE W-IDLOPNR-4114     TO W-IDLOPNR-4114-MIN               
090000           END-IF                                                         
090100           PERFORM IMS-GU-WL411301                                        
090200           PERFORM IMS-GNP-WL411311                                       
090300           PERFORM FA-LAES-VISA-INFO-ADM                                  
090400           MOVE MFS-CLOSE-FIELD    TO MOD-KDORDKL-UPPD-ATTR               
090500                                      MOD-ADLAGOMR-UPPD-ATTR              
090600        WHEN 'RET'                                                        
090700           IF MFS-UPDATE                                                  
090800              MOVE W-KDANMORS-4116    TO W-KDANMORS-4116-MIN              
090900              MOVE W-IDLOPNR-4116     TO W-IDLOPNR-4116-MIN               
091000           END-IF                                                         
091100           PERFORM IMS-GU-WL411501                                        
091200           PERFORM IMS-GNP-WL411511                                       
091300           PERFORM FB-LAES-VISA-INFO-RET                                  
091400           MOVE MFS-CLOSE-FIELD    TO MOD-KDORDKL-UPPD-ATTR               
091500                                      MOD-ADLAGOMR-UPPD-ATTR              
091600        WHEN 'REM'                                                        
091700           IF MFS-UPDATE                                                  
091800              MOVE W-KDANMORS-4118    TO W-KDANMORS-4118-MIN              
091900              MOVE W-IDDC-4118        TO W-IDDC-4118-MIN                  
092000              MOVE W-IDLOPNR-4118     TO W-IDLOPNR-4118-MIN               
092100           END-IF                                                         
092200           PERFORM IMS-GU-WL411701                                        
092300           PERFORM IMS-GNP-WL411711                                       
092400           PERFORM FC-LAES-VISA-INFO-REM                                  
092500     END-EVALUATE                                                         
092600                                                                          
092700     .                                                                    
092800     EJECT                                                                
092900 FA-LAES-VISA-INFO-ADM SECTION.                                           
093000                                                                          
093100     IF SEGMENT-SAKNAS                                                    
093200       MOVE ERR-KEY-MISSING           TO MED-IDMFSFEL                     
093300       CALL WMEDKONV USING MED-WMEDAREA                                   
093400       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
093500       PERFORM MFS-RENSA-FAELT-UT                                         
093600     ELSE                                                                 
093700       PERFORM FAA-REDIGERA-RAD-1                                         
093800       MOVE WS-KDARBTYP               TO MOD-KDARBTYP-ENTER               
093900       MOVE 4114-KDANMORS             TO MOD-KDANMORS-ENTER               
094000       MOVE 4114-IDLOPNR              TO MOD-IDRADNR-ENTER                
094100       PERFORM IMS-GNP-WL411311                                           
094200                                                                          
094300       MOVE +1 TO INDX                                                    
094400       PERFORM UNTIL INDX > MAX-INDX                                      
094500         IF SEGMENT-FINNS                                                 
094600           PERFORM FAB-REDIGERA-RAD-2-11                                  
094700           PERFORM IMS-GNP-WL411311                                       
094800         ELSE                                                             
094900           PERFORM MFS-RENSA-RAD                                          
095000         END-IF                                                           
095100         ADD 1 TO INDX                                                    
095200       END-PERFORM                                                        
095300                                                                          
095400       IF SEGMENT-FINNS                                                   
095500         MOVE WS-KDARBTYP             TO MOD-KDARBTYP-NEXT                
095600         IF 4114-KDANMORS = X'FFFF'                                       
095700            MOVE 'ÖÖ'                 TO MOD-KDANMORS-NEXT                
095800         ELSE                                                             
095900            MOVE 4114-KDANMORS        TO MOD-KDANMORS-NEXT                
096000         END-IF                                                           
096100         MOVE 4114-IDLOPNR            TO MOD-IDRADNR-NEXT                 
096200                                                                          
096300         IF MED-IDMFSINF = SPACE                                          
096400           MOVE INF-MORE-INFO-EXISTS                                      
096500                                  TO MED-IDMFSINF                         
096600           CALL WMEDKONV USING MED-WMEDAREA                               
096700           MOVE MED-MFSINF              TO MOD-TEMFSINF                   
096800         END-IF                                                           
096900       ELSE                                                               
097000         MOVE SPACE                   TO MOD-KDARBTYP-NEXT                
097100         MOVE ZERO                    TO MOD-KDANMORS-NEXT                
097200                                         MOD-IDRADNR-NEXT                 
097300         IF MED-IDMFSINF = SPACE                                          
097400           MOVE INF-LAST-PAGE         TO MED-IDMFSINF                     
097500           CALL WMEDKONV USING MED-WMEDAREA                               
097600           MOVE MED-MFSINF            TO MOD-TEMFSINF                     
097700         END-IF                                                           
097800       END-IF                                                             
097900     END-IF                                                               
098000                                                                          
098100     .                                                                    
098200     EJECT                                                                
098300 FAA-REDIGERA-RAD-1 SECTION.                                              
098400                                                                          
098500     MOVE 4114-KDANMORS               TO MOD-KDANMORS-RAD1                
098600     MOVE 4114-IDLOPNR                TO MOD-IDRADNR-RAD1                 
098700     MOVE 4114-IDDISTR-FOM            TO MOD-IDDISTR-FOM-RAD1             
098800     MOVE 4114-IDDISTR-TOM            TO MOD-IDDISTR-TOM-RAD1             
098900     MOVE 4114-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM-RAD1            
099000     MOVE 4114-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM-RAD1            
099100     MOVE 4114-KDARBTYP-ADM           TO WS-SPAR-KDARBTYP                 
099200     MOVE 4114-IDPERSON-ADM           TO WS-SPAR-IDPERSON                 
099300     IF WS-SPAR-IDPERSON = ZERO                                           
099400         MOVE ZERO                    TO WS-SPAR-IDPERSON                 
099500     END-IF                                                               
099600     MOVE WS-KDARBTYP-IDPERSON        TO MOD-ARB-PERS-RAD1                
099700     MOVE 4114-KDARBTYP-ADM           TO W-KDARBTYP                       
099800     MOVE 4114-IDPERSON-ADM           TO W-IDPERSON                       
099900     PERFORM IMS-GU-WDP311                                                
100000     IF SEGMENT-FINNS                                                     
100100       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
100200       IF WS-IDMAIL(28:1) > SPACE                                         
100300          MOVE '*' TO WS-IDMAIL(27:1)                                     
100400       END-IF                                                             
100500       MOVE WS-IDMAIL30               TO MOD-IDMAIL30-RAD1                
100600     ELSE                                                                 
100700       MOVE SPACE                     TO MOD-IDMAIL30-RAD1                
100800     END-IF                                                               
100900                                                                          
101000     IF MFS-UPDATE AND MID-FLBORT-UPPD = '+'                              
101100       PERFORM FAAA-LYS-UPP-RAD1                                          
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 FAAA-LYS-UPP-RAD1 SECTION.                                               
101600                                                                          
101700     MOVE MFS-ADD-LYS-UPP-FAELT      TO MOD-IDRADNR-RAD1-ATTR             
101800                                        MOD-KDANMORS-RAD1-ATTR            
101900                                        MOD-IDDISTR-FOM-RAD1-ATTR         
102000                                        MOD-IDDISTR-TOM-RAD1-ATTR         
102100                                        MOD-IDKUNDNR-FOM-RAD1-ATTR        
102200                                        MOD-IDKUNDNR-TOM-RAD1-ATTR        
102300                                        MOD-ARB-PERS-RAD1-ATTR            
102400                                        MOD-IDMAIL30-RAD1-ATTR            
102500     .                                                                    
102600     EJECT                                                                
102700 FAB-REDIGERA-RAD-2-11 SECTION.                                           
102800                                                                          
102900     MOVE 4114-IDLOPNR                TO MOD-IDRADNR       (INDX)         
103000     MOVE 4114-KDANMORS               TO MOD-KDANMORS      (INDX)         
103100     MOVE 4114-IDDISTR-FOM            TO MOD-IDDISTR-FOM   (INDX)         
103200     MOVE 4114-IDDISTR-TOM            TO MOD-IDDISTR-TOM   (INDX)         
103300     MOVE 4114-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM  (INDX)         
103400     MOVE 4114-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM  (INDX)         
103500     MOVE 4114-KDARBTYP-ADM           TO MOD-KDARBTYP      (INDX)         
103600     IF 4114-IDPERSON-ADM = ZERO                                          
103700         MOVE SPACE                   TO MOD-IDPERSON      (INDX)         
103800     ELSE                                                                 
103900         MOVE 4114-IDPERSON-ADM       TO WS-SPAR-IDPERSON                 
104000         MOVE WS-SPAR-IDPERSON        TO MOD-IDPERSON      (INDX)         
104100     END-IF                                                               
104200     MOVE 4114-KDARBTYP-ADM           TO W-KDARBTYP                       
104300     MOVE 4114-IDPERSON-ADM           TO W-IDPERSON                       
104400     PERFORM IMS-GU-WDP311                                                
104500     IF SEGMENT-FINNS                                                     
104600       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
104700       IF WS-IDMAIL(31:1) > SPACE                                         
104800          MOVE '*' TO WS-IDMAIL(30:1)                                     
104900       END-IF                                                             
105000       MOVE WS-IDMAIL30               TO MOD-IDMAIL30      (INDX)         
105100     ELSE                                                                 
105200       MOVE SPACE                     TO MOD-IDMAIL30      (INDX)         
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 FB-LAES-VISA-INFO-RET SECTION.                                           
105700                                                                          
105800     IF SEGMENT-SAKNAS                                                    
105900       MOVE ERR-KEY-MISSING           TO MED-IDMFSFEL                     
106000       CALL WMEDKONV USING MED-WMEDAREA                                   
106100       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
106200       PERFORM MFS-RENSA-FAELT-UT                                         
106300     ELSE                                                                 
106400       PERFORM FBA-REDIGERA-RAD-1                                         
106500       MOVE WS-KDARBTYP               TO MOD-KDARBTYP-ENTER               
106600       MOVE 4116-KDANMORS             TO MOD-KDANMORS-ENTER               
106700       MOVE 4116-IDLOPNR              TO MOD-IDRADNR-ENTER                
106800       PERFORM IMS-GNP-WL411511                                           
106900                                                                          
107000       MOVE +1 TO INDX                                                    
107100       PERFORM UNTIL INDX > MAX-INDX                                      
107200         IF SEGMENT-FINNS                                                 
107300           PERFORM FBB-REDIGERA-RAD-2-11                                  
107400           PERFORM IMS-GNP-WL411511                                       
107500         ELSE                                                             
107600           PERFORM MFS-RENSA-RAD                                          
107700         END-IF                                                           
107800         ADD 1 TO INDX                                                    
107900       END-PERFORM                                                        
108000                                                                          
108100       IF SEGMENT-FINNS                                                   
108200         MOVE WS-KDARBTYP             TO MOD-KDARBTYP-NEXT                
108300         IF 4116-KDANMORS = X'FFFF'                                       
108400            MOVE 'ÖÖ'                 TO MOD-KDANMORS-NEXT                
108500         ELSE                                                             
108600            MOVE 4116-KDANMORS        TO MOD-KDANMORS-NEXT                
108700         END-IF                                                           
108800         MOVE 4116-IDLOPNR            TO MOD-IDRADNR-NEXT                 
108900                                                                          
109000         MOVE INF-MORE-INFO-EXISTS                                        
109100                                TO MED-IDMFSINF                           
109200         CALL WMEDKONV USING MED-WMEDAREA                                 
109300         MOVE MED-TEMFSINF            TO MOD-TEMFSINF                     
109400       ELSE                                                               
109500         MOVE SPACE                   TO MOD-KDARBTYP-NEXT                
109600         MOVE ZERO                    TO MOD-KDANMORS-NEXT                
109700                                         MOD-IDRADNR-NEXT                 
109800         IF MED-IDMFSINF = SPACE                                          
109900           MOVE INF-LAST-PAGE         TO MED-IDMFSINF                     
110000         END-IF                                                           
110100       END-IF                                                             
110200     END-IF                                                               
110300                                                                          
110400     .                                                                    
110500     EJECT                                                                
110600 FBA-REDIGERA-RAD-1 SECTION.                                              
110700                                                                          
110800     MOVE 4116-KDANMORS               TO MOD-KDANMORS-RAD1                
110900     MOVE 4116-IDLOPNR                TO MOD-IDRADNR-RAD1                 
111000     MOVE 4116-IDDC                   TO MOD-IDDC-RAD1                    
111100     MOVE 4116-IDDISTR-FOM            TO MOD-IDDISTR-FOM-RAD1             
111200     MOVE 4116-IDDISTR-TOM            TO MOD-IDDISTR-TOM-RAD1             
111300     MOVE 4116-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM-RAD1            
111400     MOVE 4116-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM-RAD1            
111500     MOVE 4116-KDARBTYP-RET           TO WS-SPAR-KDARBTYP                 
111600     MOVE 4116-IDPERSON-RET           TO WS-SPAR-IDPERSON                 
111700     IF WS-SPAR-IDPERSON = ZERO                                           
111800         MOVE ZERO                    TO WS-SPAR-IDPERSON                 
111900     END-IF                                                               
112000                                                                          
112100     MOVE WS-KDARBTYP-IDPERSON        TO MOD-ARB-PERS-RAD1                
112200     MOVE 4116-KDARBTYP-RET           TO W-KDARBTYP                       
112300     MOVE 4116-IDPERSON-RET           TO W-IDPERSON                       
112400     PERFORM IMS-GU-WDP311                                                
112500     IF SEGMENT-FINNS                                                     
112600       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
112700       IF WS-IDMAIL(31:1) > SPACE                                         
112800          MOVE '*' TO WS-IDMAIL(30:1)                                     
112900       END-IF                                                             
113000       MOVE WS-IDMAIL30               TO MOD-IDMAIL30-RAD1                
113100     ELSE                                                                 
113200       MOVE SPACE                     TO MOD-IDMAIL30-RAD1                
113300     END-IF                                                               
113400                                                                          
113500     IF MFS-UPDATE AND MID-FLBORT-UPPD = '+'                              
113600       PERFORM FBAA-LYS-UPP-RAD1                                          
113700     END-IF                                                               
113800     .                                                                    
113900     EJECT                                                                
114000 FBAA-LYS-UPP-RAD1 SECTION.                                               
114100                                                                          
114200     MOVE MFS-ADD-LYS-UPP-FAELT      TO MOD-IDRADNR-RAD1-ATTR             
114300                                        MOD-KDANMORS-RAD1-ATTR            
114400                                        MOD-IDDC-RAD1-ATTR                
114500                                        MOD-IDDISTR-FOM-RAD1-ATTR         
114600                                        MOD-IDDISTR-TOM-RAD1-ATTR         
114700                                        MOD-IDKUNDNR-FOM-RAD1-ATTR        
114800                                        MOD-IDKUNDNR-TOM-RAD1-ATTR        
114900                                        MOD-ARB-PERS-RAD1-ATTR            
115000                                        MOD-IDMAIL30-RAD1-ATTR            
115100     .                                                                    
115200     EJECT                                                                
115300 FBB-REDIGERA-RAD-2-11 SECTION.                                           
115400                                                                          
115500     MOVE 4116-IDLOPNR                TO MOD-IDRADNR       (INDX)         
115600     MOVE 4116-KDANMORS               TO MOD-KDANMORS      (INDX)         
115700     MOVE 4116-IDDC                   TO MOD-IDDC          (INDX)         
115800     MOVE 4116-IDDISTR-FOM            TO MOD-IDDISTR-FOM   (INDX)         
115900     MOVE 4116-IDDISTR-TOM            TO MOD-IDDISTR-TOM   (INDX)         
116000     MOVE 4116-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM  (INDX)         
116100     MOVE 4116-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM  (INDX)         
116200     MOVE 4116-KDARBTYP-RET           TO MOD-KDARBTYP      (INDX)         
116300     IF 4116-IDPERSON-RET = ZERO                                          
116400         MOVE SPACE                   TO MOD-IDPERSON      (INDX)         
116500     ELSE                                                                 
116600         MOVE 4116-IDPERSON-RET       TO WS-SPAR-IDPERSON                 
116700         MOVE WS-SPAR-IDPERSON        TO MOD-IDPERSON      (INDX)         
116800     END-IF                                                               
116900     MOVE 4116-KDARBTYP-RET           TO W-KDARBTYP                       
117000     MOVE 4116-IDPERSON-RET           TO W-IDPERSON                       
117100     PERFORM IMS-GU-WDP311                                                
117200     IF SEGMENT-FINNS                                                     
117300       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
117400       IF WS-IDMAIL(31:1) > SPACE                                         
117500          MOVE '*' TO WS-IDMAIL(30:1)                                     
117600       END-IF                                                             
117700       MOVE WS-IDMAIL30               TO MOD-IDMAIL30      (INDX)         
117800     ELSE                                                                 
117900       MOVE SPACE                     TO MOD-IDMAIL30      (INDX)         
118000     END-IF                                                               
118100     .                                                                    
118200     EJECT                                                                
118300 FC-LAES-VISA-INFO-REM SECTION.                                           
118400                                                                          
118500     IF SEGMENT-SAKNAS                                                    
118600       MOVE ERR-KEY-MISSING           TO MED-IDMFSFEL                     
118700       CALL WMEDKONV USING MED-WMEDAREA                                   
118800       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
118900       PERFORM MFS-RENSA-FAELT-UT                                         
119000     ELSE                                                                 
119100       PERFORM FCA-REDIGERA-RAD-1                                         
119200       MOVE WS-KDARBTYP               TO MOD-KDARBTYP-ENTER               
119300       MOVE 4118-KDANMORS             TO MOD-KDANMORS-ENTER               
119400       MOVE 4118-IDDC                 TO MOD-IDDC-ENTER                   
119500       MOVE 4118-IDLOPNR              TO MOD-IDRADNR-ENTER                
119600       PERFORM IMS-GNP-WL411711                                           
119700                                                                          
119800       MOVE +1 TO INDX                                                    
119900       PERFORM UNTIL INDX > MAX-INDX                                      
120000         IF SEGMENT-FINNS                                                 
120100           PERFORM FCB-REDIGERA-RAD-2-11                                  
120200           PERFORM IMS-GNP-WL411711                                       
120300         ELSE                                                             
120400           PERFORM MFS-RENSA-RAD                                          
120500         END-IF                                                           
120600         ADD 1 TO INDX                                                    
120700       END-PERFORM                                                        
120800                                                                          
120900       IF SEGMENT-FINNS                                                   
121000         MOVE WS-KDARBTYP             TO MOD-KDARBTYP-NEXT                
121100         IF 4118-KDANMORS = X'FFFF'                                       
121200            MOVE 'ÖÖ'                 TO MOD-KDANMORS-NEXT                
121300         ELSE                                                             
121400            MOVE 4118-KDANMORS        TO MOD-KDANMORS-NEXT                
121500         END-IF                                                           
121600         MOVE 4118-IDDC               TO MOD-IDDC-NEXT                    
121700         MOVE 4118-IDLOPNR            TO MOD-IDRADNR-NEXT                 
121800                                                                          
121900         IF MED-IDMFSINF = SPACE                                          
122000         MOVE INF-MORE-INFO-EXISTS                                        
122100                                TO MED-IDMFSINF                           
122200         CALL WMEDKONV USING MED-WMEDAREA                                 
122300         MOVE MED-TEMFSINF            TO MOD-TEMFSINF                     
122400         END-IF                                                           
122500       ELSE                                                               
122600         MOVE SPACE                   TO MOD-KDARBTYP-NEXT                
122700                                         MOD-IDDC-NEXT                    
122800         MOVE ZERO                    TO MOD-KDANMORS-NEXT                
122900                                         MOD-IDRADNR-NEXT                 
123000         IF MED-IDMFSINF = SPACE                                          
123100           MOVE INF-LAST-PAGE         TO MED-IDMFSINF                     
123200         END-IF                                                           
123300       END-IF                                                             
123400     END-IF                                                               
123500                                                                          
123600     .                                                                    
123700     EJECT                                                                
123800 FCA-REDIGERA-RAD-1 SECTION.                                              
123900                                                                          
124000     MOVE 4118-KDANMORS               TO MOD-KDANMORS-RAD1                
124100     MOVE 4118-IDLOPNR                TO MOD-IDRADNR-RAD1                 
124200     MOVE 4118-IDDC                   TO MOD-IDDC-RAD1                    
124300     MOVE 4118-KDORDKL                TO MOD-KDORDKL-RAD1                 
124400     MOVE 4118-ADLAGOMR               TO MOD-ADLAGOMR-RAD1                
124500     MOVE 4118-IDDISTR-FOM            TO MOD-IDDISTR-FOM-RAD1             
124600     MOVE 4118-IDDISTR-TOM            TO MOD-IDDISTR-TOM-RAD1             
124700     MOVE 4118-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM-RAD1            
124800     MOVE 4118-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM-RAD1            
124900     MOVE 4118-KDARBTYP-REM           TO WS-SPAR-KDARBTYP                 
125000     MOVE 4118-IDPERSON-REM           TO WS-SPAR-IDPERSON                 
125100     IF WS-SPAR-IDPERSON = ZERO                                           
125200         MOVE ZERO                    TO WS-SPAR-IDPERSON                 
125300     END-IF                                                               
125400                                                                          
125500     MOVE WS-KDARBTYP-IDPERSON        TO MOD-ARB-PERS-RAD1                
125600     MOVE 4118-KDARBTYP-REM           TO W-KDARBTYP                       
125700     MOVE 4118-IDPERSON-REM           TO W-IDPERSON                       
125800     PERFORM IMS-GU-WDP311                                                
125900     IF SEGMENT-FINNS                                                     
126000       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
126100       IF WS-IDMAIL(31:1) > SPACE                                         
126200          MOVE '*' TO WS-IDMAIL(30:1)                                     
126300       END-IF                                                             
126400       MOVE WS-IDMAIL30               TO MOD-IDMAIL30-RAD1                
126500     ELSE                                                                 
126600       MOVE SPACE                     TO MOD-IDMAIL30-RAD1                
126700     END-IF                                                               
126800                                                                          
126900     IF MFS-UPDATE AND MID-FLBORT-UPPD = '+'                              
127000       PERFORM FCAA-LYS-UPP-RAD1                                          
127100     END-IF                                                               
127200     .                                                                    
127300     EJECT                                                                
127400 FCAA-LYS-UPP-RAD1 SECTION.                                               
127500                                                                          
127600     MOVE MFS-ADD-LYS-UPP-FAELT      TO MOD-IDRADNR-RAD1-ATTR             
127700                                        MOD-KDANMORS-RAD1-ATTR            
127800                                        MOD-IDDC-RAD1-ATTR                
127810                                        MOD-KDORDKL-RAD1-ATTR             
127900                                        MOD-ADLAGOMR-RAD1-ATTR            
128000                                        MOD-IDDISTR-FOM-RAD1-ATTR         
128100                                        MOD-IDDISTR-TOM-RAD1-ATTR         
128200                                        MOD-IDKUNDNR-FOM-RAD1-ATTR        
128300                                        MOD-IDKUNDNR-TOM-RAD1-ATTR        
128400                                        MOD-ARB-PERS-RAD1-ATTR            
128500                                        MOD-IDMAIL30-RAD1-ATTR            
128600     .                                                                    
128700     EJECT                                                                
128800 FCB-REDIGERA-RAD-2-11 SECTION.                                           
128900                                                                          
129000     MOVE 4118-IDLOPNR                TO MOD-IDRADNR       (INDX)         
129100     MOVE 4118-KDANMORS               TO MOD-KDANMORS      (INDX)         
129200     MOVE 4118-IDDC                   TO MOD-IDDC          (INDX)         
129300     MOVE 4118-KDORDKL                TO MOD-KDORDKL       (INDX)         
129400     MOVE 4118-ADLAGOMR               TO MOD-ADLAGOMR      (INDX)         
129500     MOVE 4118-IDDISTR-FOM            TO MOD-IDDISTR-FOM   (INDX)         
129600     MOVE 4118-IDDISTR-TOM            TO MOD-IDDISTR-TOM   (INDX)         
129700     MOVE 4118-IDKUNDNR-FOM           TO MOD-IDKUNDNR-FOM  (INDX)         
129800     MOVE 4118-IDKUNDNR-TOM           TO MOD-IDKUNDNR-TOM  (INDX)         
129900     MOVE 4118-KDARBTYP-REM           TO MOD-KDARBTYP      (INDX)         
130000     IF 4118-IDPERSON-REM = ZERO                                          
130100         MOVE SPACE                   TO MOD-IDPERSON      (INDX)         
130200     ELSE                                                                 
130300         MOVE 4118-IDPERSON-REM       TO WS-SPAR-IDPERSON                 
130400         MOVE WS-SPAR-IDPERSON        TO MOD-IDPERSON      (INDX)         
130500     END-IF                                                               
130600     MOVE 4118-KDARBTYP-REM           TO W-KDARBTYP                       
130700     MOVE 4118-IDPERSON-REM           TO W-IDPERSON                       
130800     PERFORM IMS-GU-WDP311                                                
130900     IF SEGMENT-FINNS                                                     
131000       MOVE PERS-IDMAIL TO WS-IDMAIL                                      
131100       IF WS-IDMAIL(31:1) > SPACE                                         
131200          MOVE '*' TO WS-IDMAIL(30:1)                                     
131300       END-IF                                                             
131400       MOVE WS-IDMAIL30               TO MOD-IDMAIL30      (INDX)         
131500     ELSE                                                                 
131600       MOVE SPACE                     TO MOD-IDMAIL30      (INDX)         
131700     END-IF                                                               
131800     .                                                                    
131900     EJECT                                                                
132000 G-KOLLA-INPUT SECTION.                                                   
132100                                                                          
132200     MOVE JA                          TO INDATA-SW                        
132300     IF MID-INPUT = ALL '+'                                               
132400       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
132500       CALL WMEDKONV USING MED-WMEDAREA                                   
132600       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
132700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
132800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
132900       MOVE NEJ                       TO INDATA-SW                        
133000     ELSE                                                                 
133100      IF WS-KDARBTYP = 'REM'                                              
133200* EFTER TILLÄGG AV KL + LO PÅ UPPD-RAD FÖR "REM" GÅR DET INTE             
133300* LÄNGRE ATT BLANDA IHOP KONTROLLERNA MED "ADM" OCH "RET".                
133400* DÄRFÖR LYFTER JAG UT ALLT SOM HAR MED "REM"-KOLL ATT GÖRA I             
133500* SEPARATA GX-SEKTIONER. /LASSI (SCR 8687963)                             
133700                                                                          
133800        PERFORM GX-KOLL-UPP-RAD-REM                                       
133900      ELSE                                                                
134000                                                                          
134100       PERFORM GA-FORMELL-KOLL-UPP-RAD                                    
134200                                                                          
134300       IF INDATA-FEL                                                      
134400         IF MED-IDMFSFEL = SPACE                                          
134500           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
134600         END-IF                                                           
134700         CALL WMEDKONV USING MED-WMEDAREA                                 
134800         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
134900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
135000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
135100       ELSE                                                               
135200         IF MID-FLBORT-UPPD = 'J' OR 'Y'                                  
135300           PERFORM GB-KOLLA-OM-BORTTAG-OK                                 
135400         ELSE                                                             
135500           PERFORM GC-UPPD-RAD-LOGISK-KOLL                                
135600           IF AENDRA-OK                                                   
135700               PERFORM GD-KOLLA-KOD-RADNR                                 
135800           END-IF                                                         
135900         END-IF                                                           
136000                                                                          
136100         IF INDATA-FEL                                                    
136200           IF MED-IDMFSFEL = SPACE                                        
136300             MOVE ERR-CORR-HILITE-FLDS                                    
136400                                      TO MED-IDMFSFEL                     
136500           END-IF                                                         
136600           CALL WMEDKONV USING MED-WMEDAREA                               
136700           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
136800           PERFORM MFS-ROER-EJ-FAELT-IN                                   
136900           PERFORM MFS-ROER-EJ-FAELT-UT                                   
137000         END-IF                                                           
137100       END-IF                                                             
137200      END-IF                                                              
137300     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137608 GX-KOLL-UPP-RAD-REM SECTION.                                             
137700                                                                          
137809     IF MID-FLBORT-UPPD NOT = '+'                                         
137909       IF MID-FLBORT-UPPD = 'Y' OR 'J'                                    
138009         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-UPPD-ATTR                
138109         MOVE JA                   TO SW-BORT                             
138209       ELSE                                                               
138309         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBORT-UPPD-ATTR                
138409         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
138509         MOVE NEJ                  TO INDATA-SW                           
138609       END-IF                                                             
138709     ELSE                                                                 
138809       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLBORT-UPPD-ATTR                
138910       MOVE NEJ                    TO SW-BORT                             
139009     END-IF                                                               
139109                                                                          
139200     PERFORM GXA-KOLLA-IDRADNR                                            
139300     PERFORM GXB-KOLLA-KDANMORS                                           
139400     PERFORM S01-KOLLA-IDDC                                               
139410     IF MID-FLBORT-UPPD = '+'                                             
139500       PERFORM GXC-KOLLA-IDDISTR                                          
139600       PERFORM GXD-KOLLA-IDKUNDNR                                         
139700       PERFORM GXG-KOLLA-KDORDKL                                          
139800       PERFORM GXH-KOLLA-ADLAGOMR                                         
139906       PERFORM GXE-KOLLA-IDPERS                                           
139907     END-IF                                                               
140008                                                                          
141009     IF INDATA-OK AND INGET-BORTTAG                                       
141609       PERFORM GXI-UPPD-RAD-LOGISK-KOLL                                   
141709       IF DUPLICATE                                                       
141809         MOVE ERR-DUPLICATE-RECORD TO MED-IDMFSFEL                        
141909         MOVE NEJ                  TO INDATA-SW                           
142009       END-IF                                                             
142109     END-IF                                                               
142209                                                                          
142608     IF INDATA-FEL                                                        
142708       IF MED-IDMFSFEL = SPACE                                            
142809         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
143008       END-IF                                                             
143108       CALL WMEDKONV USING MED-WMEDAREA                                   
143208       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
143308       PERFORM MFS-ROER-EJ-FAELT-IN                                       
143408       PERFORM MFS-ROER-EJ-FAELT-UT                                       
143508     END-IF                                                               
143608     .                                                                    
143708     EJECT                                                                
143808 GXA-KOLLA-IDRADNR SECTION.                                               
143908     SKIP2                                                                
144008     IF MID-IDRADNR-UPPD NOT = ALL '+'                                    
144108       IF MID-IDRADNR-UPPD NOT NUMERIC                                    
144208         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDRADNR-UPPD-ATTR            
144308         MOVE NEJ                     TO INDATA-SW                        
144408         MOVE '002   '                TO MED-IDMFSFEL                     
144508       ELSE                                                               
144608         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDRADNR-UPPD-ATTR            
144708         MOVE MID-IDRADNR-UPPD        TO W-IDLOPNR-4118                   
144808       END-IF                                                             
144908     ELSE                                                                 
145008       IF MID-FLBORT-UPPD = 'Y' OR 'J'                                    
145108         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDRADNR-UPPD-ATTR            
145208         MOVE ERR-NOT-NUMERIC         TO MED-IDMFSFEL                     
145308         MOVE NEJ                     TO INDATA-SW                        
145408       ELSE                                                               
145508         MOVE JA                   TO SW-NYTT                             
145608         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDRADNR-UPPD-ATTR               
145808       END-IF                                                             
145908     END-IF                                                               
146008     .                                                                    
146108     EJECT                                                                
146208 GXB-KOLLA-KDANMORS SECTION.                                              
146308     SKIP2                                                                
146408     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
147000       IF MID-KDANMORS-UPPD NOT NUMERIC                                   
147106         IF MID-KDANMORS-UPPD = 'XX'                                      
147206** DEFAULT-RAD OK BARA FÖR ÄNDRING AV REM-ANSVARIG!                       
147306           IF (MID-INPUT (8:20) = ALL '+' AND                             
147410               MID-KDARB-IDPERS-UPPD NOT = ALL '+') AND                   
147510               MID-FLBORT-UPPD = '+'                                      
147706             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDANMORS-UPPD-ATTR        
147806             MOVE MID-KDANMORS-UPPD      TO W-KDANMORS-4118               
147906           ELSE                                                           
148006             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDANMORS-UPPD-ATTR          
148106             MOVE ERR-UPDATE-NOT-OK    TO MED-IDMFSFEL                    
148206             MOVE NEJ                  TO INDATA-SW                       
148306           END-IF                                                         
148406         END-IF                                                           
148506       ELSE                                                               
148606         MOVE MID-KDANMORS-UPPD       TO LKOD-KDANMORS                    
148706         CALL W418OKOD USING LKOD-KDANMORS                                
148806         IF LKOD-FL-GODK-KOD = JA     OR                                  
148906            MID-KDANMORS-UPPD = '97'  OR                                  
149006            MID-KDANMORS-UPPD = HIGH-VALUE                                
149106           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDANMORS-UPPD-ATTR           
149206           MOVE MID-KDANMORS-UPPD     TO W-KDANMORS-4118                  
149306         ELSE                                                             
149406           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDANMORS-UPPD-ATTR           
149506           MOVE ERR-WRONG-CODE        TO MED-IDMFSFEL                     
149606           CALL WMEDKONV USING MED-WMEDAREA                               
149706           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
149806           MOVE NEJ                   TO INDATA-SW                        
149906         END-IF                                                           
150006       END-IF                                                             
150106     ELSE                                                                 
150206       MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDANMORS-UPPD-ATTR          
150306       MOVE ERR-UPDATE-NOT-OK          TO MED-IDMFSFEL                    
150406       MOVE NEJ                        TO INDATA-SW                       
150800     END-IF                                                               
150900     .                                                                    
151000     EJECT                                                                
151100 GXC-KOLLA-IDDISTR SECTION.                                               
151200     SKIP2                                                                
151300     IF MID-IDDISTR-FOM-UPPD NOT = ALL '+'                                
151400       IF MID-IDDISTR-FOM-UPPD NOT NUMERIC                                
151500         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-UPPD-ATTR        
151600         MOVE ERR-NOT-NUMERIC         TO MED-IDMFSFEL                     
151700         MOVE NEJ                     TO INDATA-SW                        
151800       ELSE                                                               
151900         IF MID-IDDISTR-TOM-UPPD NOT = ALL '+'                            
152000           IF MID-IDDISTR-TOM-UPPD NUMERIC              AND               
152108              MID-IDDISTR-FOM-UPPD > +0                 AND               
152308             ((MID-IDDISTR-TOM-UPPD > MID-IDDISTR-FOM-UPPD) OR            
152408              (MID-IDDISTR-TOM-UPPD = MID-IDDISTR-FOM-UPPD))              
152508             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-UPPD-ATTR        
152608                                         MOD-IDDISTR-FOM-UPPD-ATTR        
152708             MOVE MID-IDDISTR-FOM-UPPD TO W-IDDISTRF                      
152808             MOVE MID-IDDISTR-TOM-UPPD TO W-IDDISTRT                      
152908           ELSE                                                           
153008             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-UPPD-ATTR        
153111             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-UPPD-ATTR        
153208             MOVE ERR-TOM-LESS-THAN-FOM                                   
153308                                      TO MED-IDMFSFEL                     
153408             MOVE NEJ                 TO INDATA-SW                        
153508           END-IF                                                         
153608         ELSE                                                             
153708           IF NYTT                                                        
153808             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-UPPD-ATTR        
153908             MOVE ERR-SPECIFY-ONE-CHOICE TO MED-IDMFSFEL                  
154008             MOVE NEJ                 TO INDATA-SW                        
154108           ELSE                                                           
154208             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-UPPD-ATTR        
154308                                         MOD-IDDISTR-TOM-UPPD-ATTR        
154309             MOVE MID-IDDISTR-FOM-UPPD TO W-IDDISTRF                      
154408           END-IF                                                         
154508         END-IF                                                           
154608       END-IF                                                             
154708     ELSE                                                                 
154808       IF NYTT                                                            
154908         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-UPPD-ATTR        
155217         IF MID-IDDISTR-TOM-UPPD = ALL '+'                                
155317           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-UPPD-ATTR        
155318         ELSE                                                             
155319           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-TOM-UPPD-ATTR        
155517         END-IF                                                           
155617         MOVE ERR-SPECIFY-ONE-CHOICE  TO MED-IDMFSFEL                     
155717         MOVE NEJ                     TO INDATA-SW                        
155808       ELSE                                                               
155908         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDDISTR-FOM-UPPD-ATTR        
156017         IF MID-IDDISTR-TOM-UPPD NOT = ALL '+'                            
156117           IF MID-IDDISTR-TOM-UPPD NUMERIC                                
156517             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-UPPD-ATTR        
156817             MOVE MID-IDDISTR-TOM-UPPD TO W-IDDISTRT                      
156917           ELSE                                                           
157017             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-UPPD-ATTR        
157117             MOVE ERR-NOT-NUMERIC       TO MED-IDMFSFEL                   
157417             MOVE NEJ                   TO INDATA-SW                      
157517           END-IF                                                         
157518         ELSE                                                             
157519           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-TOM-UPPD-ATTR        
157520         END-IF                                                           
157608       END-IF                                                             
157708     END-IF                                                               
157808     .                                                                    
157908     EJECT                                                                
158008 GXD-KOLLA-IDKUNDNR SECTION.                                              
158108     SKIP2                                                                
158208     IF MID-IDKUNDNR-FOM-UPPD NOT = ALL '+'                               
158308       IF MID-IDKUNDNR-FOM-UPPD NOT NUMERIC                               
158408         MOVE MFS-NUM-FAELT-FEL       TO                                  
158508                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
158608         MOVE ERR-NOT-NUMERIC         TO MED-IDMFSFEL                     
158708         MOVE NEJ                     TO INDATA-SW                        
158808       ELSE                                                               
158908         IF MID-IDKUNDNR-TOM-UPPD NOT = ALL '+'                           
159008           IF MID-IDKUNDNR-TOM-UPPD NUMERIC             AND               
159108             (MID-IDKUNDNR-TOM-UPPD > MID-IDKUNDNR-FOM-UPPD OR            
159208              MID-IDKUNDNR-TOM-UPPD = MID-IDKUNDNR-FOM-UPPD)              
159816             IF  MID-IDDISTR-FOM-UPPD NOT = ALL '+'  AND                  
159817                 MID-IDDISTR-TOM-UPPD NOT = ALL '+'  AND                  
159818               ((MID-IDDISTR-FOM-UPPD < MID-IDDISTR-TOM-UPPD  AND         
159916                 MID-IDKUNDNR-FOM-UPPD = ZERO                 AND         
160016                 MID-IDKUNDNR-TOM-UPPD = 999999)    OR                    
160017                (MID-IDDISTR-FOM-UPPD = MID-IDDISTR-TOM-UPPD))            
160117               MOVE MFS-NUM-FAELT-RAETT TO                                
160217                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
160317                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
160417               MOVE MID-IDKUNDNR-FOM-UPPD TO W-IDKUNDNF                   
160517               MOVE MID-IDKUNDNR-TOM-UPPD TO W-IDKUNDNT                   
160708             ELSE                                                         
160808               MOVE MFS-NUM-FAELT-FEL TO                                  
160908                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
161108                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
161208               MOVE ERR-INTERVAL      TO MED-IDMFSFEL                     
161308               MOVE NEJ               TO INDATA-SW                        
161408             END-IF                                                       
161508           ELSE                                                           
161608             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-TOM-UPPD-ATTR         
161908                                       MOD-IDKUNDNR-FOM-UPPD-ATTR         
162008             MOVE ERR-INVALID-VALUE   TO MED-IDMFSFEL                     
162208             MOVE NEJ                 TO INDATA-SW                        
162308           END-IF                                                         
162408         ELSE                                                             
162508           IF NYTT                                                        
162608             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-TOM-UPPD-ATTR         
162708             MOVE ERR-KEY-MISSING   TO MED-IDMFSFEL                       
162808             MOVE NEJ TO INDATA-SW                                        
162908           ELSE                                                           
163008            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-UPPD-ATTR        
163208           END-IF                                                         
163209           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-UPPD-ATTR         
163210           MOVE MID-IDKUNDNR-FOM-UPPD TO W-IDKUNDNF                       
163308         END-IF                                                           
163408       END-IF                                                             
163508     ELSE                                                                 
163608       IF NYTT                                                            
163708         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-UPPD-ATTR         
163709         IF MID-IDKUNDNR-TOM-UPPD = ALL '+'                               
163710           MOVE MFS-NUM-FAELT-FEL   TO  MOD-IDKUNDNR-TOM-UPPD-ATTR        
163720         ELSE                                                             
163721          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-TOM-UPPD-ATTR        
163730         END-IF                                                           
163808         MOVE ERR-KEY-MISSING       TO MED-IDMFSFEL                       
163908         MOVE NEJ TO INDATA-SW                                            
164008       ELSE                                                               
164009         IF MID-IDKUNDNR-TOM-UPPD NOT = ALL '+'                           
164010           IF MID-IDKUNDNR-TOM-UPPD NUMERIC                               
164108             MOVE MFS-NUM-FAELT-RAETT   TO                                
164109                                       MOD-IDKUNDNR-TOM-UPPD-ATTR         
164110             MOVE MID-IDKUNDNR-TOM-UPPD TO W-IDKUNDNT                     
164120           ELSE                                                           
164121             MOVE MFS-NUM-FAELT-FEL TO  MOD-IDKUNDNR-TOM-UPPD-ATTR        
164122             MOVE ERR-NOT-NUMERIC   TO MED-IDMFSFEL                       
164123             MOVE NEJ TO INDATA-SW                                        
164130           END-IF                                                         
164140         ELSE                                                             
164150          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-TOM-UPPD-ATTR        
164160         END-IF                                                           
164170         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKUNDNR-FOM-UPPD-ATTR        
164208       END-IF                                                             
164308     END-IF                                                               
165208     .                                                                    
165308     EJECT                                                                
165400 GXG-KOLLA-KDORDKL  SECTION.                                              
165500     SKIP2                                                                
165600     IF MID-KDORDKL-UPPD NOT = ALL '+'                                    
165700       IF MID-KDORDKL-UPPD NOT NUMERIC                                    
165800         MOVE MFS-NUM-FAELT-FEL    TO MOD-KDORDKL-UPPD-ATTR               
165900         MOVE ERR-NOT-NUMERIC      TO MED-IDMFSFEL                        
166000         MOVE NEJ                  TO INDATA-SW                           
166100       ELSE                                                               
166200         IF MID-KDORDKL-UPPD NOT = 0 AND                                  
166300            MID-KDORDKL-UPPD NOT = 1 AND                                  
166400            MID-KDORDKL-UPPD NOT = 2 AND                                  
166500            MID-KDORDKL-UPPD NOT = 3 AND                                  
166600            MID-KDORDKL-UPPD NOT = 4 AND                                  
166700            MID-KDORDKL-UPPD NOT = 9                                      
166800           MOVE MFS-NUM-FAELT-FEL    TO MOD-KDORDKL-UPPD-ATTR             
166900           MOVE ERR-INVALID-VALUE    TO MED-IDMFSFEL                      
167000           MOVE NEJ                  TO INDATA-SW                         
167100         ELSE                                                             
167200           MOVE MID-KDORDKL-UPPD     TO W-KDORDKL                         
167300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDORDKL-UPPD-ATTR             
167400         END-IF                                                           
167500       END-IF                                                             
167600     ELSE                                                                 
167702       IF NYTT                                                            
167800         MOVE MFS-NUM-FAELT-FEL    TO MOD-KDORDKL-UPPD-ATTR               
167900         MOVE ERR-KEY-MISSING      TO MED-IDMFSFEL                        
168000         MOVE NEJ TO INDATA-SW                                            
168108       ELSE                                                               
168109         IF MID-KDORDKL-UPPD NOT = ALL '+'                                
168208           MOVE MID-KDORDKL-UPPD   TO W-KDORDKL                           
168209         END-IF                                                           
168308         MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDORDKL-UPPD-ATTR               
168400       END-IF                                                             
168500     END-IF                                                               
168600     .                                                                    
168700 GXH-KOLLA-ADLAGOMR SECTION.                                              
168800     SKIP2                                                                
168900     IF MID-ADLAGOMR-UPPD NOT = ALL '+'                                   
169000       IF MID-ADLAGOMR-UPPD NOT NUMERIC                                   
169100         MOVE MFS-NUM-FAELT-FEL    TO MOD-ADLAGOMR-UPPD-ATTR              
169200         MOVE ERR-NOT-NUMERIC      TO MED-IDMFSFEL                        
169300         MOVE NEJ                  TO INDATA-SW                           
169400       ELSE                                                               
169500         MOVE MID-ADLAGOMR-UPPD    TO W-ADLAGOMR                          
169600         MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADLAGOMR-UPPD-ATTR              
169700       END-IF                                                             
169800     ELSE                                                                 
169902       IF NYTT                                                            
170000         MOVE MFS-NUM-FAELT-FEL    TO MOD-ADLAGOMR-UPPD-ATTR              
170100         MOVE ERR-KEY-MISSING      TO MED-IDMFSFEL                        
170200         MOVE NEJ TO INDATA-SW                                            
170308       ELSE                                                               
170309         IF MID-ADLAGOMR-UPPD NOT = ALL '+'                               
170408            MOVE MID-ADLAGOMR-UPPD    TO W-ADLAGOMR                       
170409         END-IF                                                           
170508         MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADLAGOMR-UPPD-ATTR              
170600       END-IF                                                             
170700     END-IF                                                               
170800     .                                                                    
170900     EJECT                                                                
171006 GXE-KOLLA-IDPERS SECTION.                                                
171106     SKIP2                                                                
171206     IF MID-KDARB-IDPERS-UPPD NOT = ALL '+'                               
171306       IF MID-KDARB-IDPERS-UPPD (1:3) = 'REM' AND                         
171406          MID-KDARB-IDPERS-UPPD (4:3) NUMERIC                             
171506         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARB-IDPERS-UPD-ATTR           
171606       ELSE                                                               
171706         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARB-IDPERS-UPD-ATTR           
171806         MOVE NEJ TO INDATA-SW                                            
171906       END-IF                                                             
172006     ELSE                                                                 
172106       IF NYTT                                                            
172206         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARB-IDPERS-UPD-ATTR           
172306         MOVE ERR-KEY-MISSING      TO MED-IDMFSFEL                        
172406         MOVE NEJ TO INDATA-SW                                            
172506       ELSE                                                               
172608         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARB-IDPERS-UPD-ATTR           
172706       END-IF                                                             
172806     END-IF                                                               
172906     .                                                                    
173006     EJECT                                                                
173106 GXI-UPPD-RAD-LOGISK-KOLL SECTION.                                        
173206                                                                          
173317     IF EJ-NYTT                                                           
173417       PERFORM IMS-GHU-WL411711                                           
173517       IF SEGMENT-FINNS                                                   
173617         IF MID-KDORDKL-UPPD = ALL '+'                                    
173717           MOVE 4118-KDORDKL       TO W-KDORDKL                           
173817         END-IF                                                           
173917         IF MID-ADLAGOMR-UPPD = ALL '+'                                   
174017           MOVE 4118-ADLAGOMR      TO W-ADLAGOMR                          
174117         END-IF                                                           
174218         IF MID-IDDISTR-FOM-UPPD = ALL '+'                                
174317           MOVE 4118-IDDISTR-FOM   TO W-IDDISTRF                          
174417         END-IF                                                           
174518         IF MID-IDDISTR-TOM-UPPD = ALL '+'                                
174617           MOVE 4118-IDDISTR-TOM   TO W-IDDISTRT                          
174717         END-IF                                                           
174818         IF MID-IDKUNDNR-FOM-UPPD = ALL '+'                               
174917           MOVE 4118-IDKUNDNR-FOM  TO W-IDKUNDNF                          
175017         END-IF                                                           
175118         IF MID-IDKUNDNR-TOM-UPPD = ALL '+'                               
175217           MOVE 4118-IDKUNDNR-TOM  TO W-IDKUNDNT                          
175317         END-IF                                                           
175417       ELSE                                                               
175517         MOVE NEJ TO INDATA-SW                                            
175617       END-IF                                                             
175717     END-IF                                                               
175718     IF INDATA-OK                                                         
175719       PERFORM GXIA-KOLL-DUPLICATE                                        
175720     ELSE                                                                 
175721       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDRADNR-UPPD-ATTR              
175722       MOVE ERR-KEY-MISSING  TO MED-IDMFSFEL                              
175730     END-IF                                                               
175731     .                                                                    
175732     EJECT                                                                
175740 GXIA-KOLL-DUPLICATE SECTION.                                             
175750                                                                          
175817     PERFORM IMS-GU-WL411701                                              
175917     PERFORM IMS-GNP-WL411711-KOD-DC                                      
176017     MOVE NEJ                      TO DUPLICATE-SW                        
176117     PERFORM UNTIL SEGMENT-SAKNAS  OR DUPLICATE                           
176217       IF ((4118-IDKUNDNR-FOM <= W-IDKUNDNF   AND                         
176317           4118-IDKUNDNR-TOM  >= W-IDKUNDNF)  OR                          
176417           (4118-IDKUNDNR-FOM <= W-IDKUNDNT   AND                         
176517           4118-IDKUNDNR-TOM  >= W-IDKUNDNT)  OR                          
176617           (4118-IDKUNDNR-FOM >= W-IDKUNDNF   AND                         
176717           4118-IDKUNDNR-FOM  <= W-IDKUNDNT)  OR                          
176817           (4118-IDKUNDNR-TOM >= W-IDKUNDNF   AND                         
176917           4118-IDKUNDNR-TOM  <= W-IDKUNDNT))                             
177017           AND                                                            
177117           ((4118-IDDISTR-FOM <= W-IDDISTRF   AND                         
177217           4118-IDDISTR-TOM   >= W-IDDISTRF)  OR                          
177317           (4118-IDDISTR-FOM  <= W-IDDISTRT   AND                         
177417           4118-IDDISTR-TOM   >= W-IDDISTRT)  OR                          
177517           (4118-IDDISTR-FOM  >= W-IDDISTRF   AND                         
177617           4118-IDDISTR-FOM   <= W-IDDISTRT)  OR                          
177717           (4118-IDDISTR-TOM  >= W-IDDISTRF   AND                         
177817           4118-IDDISTR-TOM   <= W-IDDISTRT))                             
177917           AND                                                            
178017           (4118-ADLAGOMR     = W-ADLAGOMR    OR                          
178117            4118-ADLAGOMR     = 0)                                        
178217           AND                                                            
178317           (4118-KDORDKL      = W-KDORDKL     OR                          
178417            4118-KDORDKL      = 9)                                        
178517         MOVE MID-KDARB-IDPERS-UPPD   TO WS-KDARBTYP-IDPERSON             
178617         IF 4118-KDARBTYP-REM = WS-SPAR-KDARBTYP AND                      
178717            4118-IDPERSON-REM = WS-SPAR-IDPERSON                          
178817           MOVE JA                    TO DUPLICATE-SW                     
178917         ELSE                                                             
179017           PERFORM IMS-GNP-WL411711-KOD-DC                                
179117         END-IF                                                           
179217       ELSE                                                               
179317         PERFORM IMS-GNP-WL411711-KOD-DC                                  
179417       END-IF                                                             
179517     END-PERFORM                                                          
179617     .                                                                    
179717     EJECT                                                                
179817 GA-FORMELL-KOLL-UPP-RAD SECTION.                                         
179917                                                                          
180017     IF MID-FLBORT-UPPD = ALL '+'                                         
180117         PERFORM GAA-KOLLA-IDRADNR                                        
180217         PERFORM GAB-KOLLA-KDANMORS                                       
180317                                                                          
180417         EVALUATE WS-KDARBTYP                                             
180517            WHEN 'ADM'                                                    
180617               PERFORM IMS-GU-WL411311                                    
180717            WHEN 'RET'                                                    
180817               PERFORM IMS-GU-WL411511                                    
180917               PERFORM S01-KOLLA-IDDC                                     
181017         END-EVALUATE                                                     
181117                                                                          
181217         PERFORM GAC-KOLLA-IDDISTR                                        
181317         PERFORM GAD-KOLLA-IDKUNDNR                                       
181417     ELSE                                                                 
181517         IF MID-FLBORT-UPPD = 'Y' OR 'J'                                  
181617           PERFORM GAA-KOLLA-IDRADNR                                      
181717           PERFORM GAB-KOLLA-KDANMORS                                     
181817           IF WS-KDARBTYP = 'RET'                                         
181917             PERFORM S01-KOLLA-IDDC                                       
182017           END-IF                                                         
182117         ELSE                                                             
182217            MOVE NEJ                  TO INDATA-SW                        
182317            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
182417         END-IF                                                           
182517     END-IF                                                               
182617     .                                                                    
182717     EJECT                                                                
182817 GAA-KOLLA-IDRADNR SECTION.                                               
182917     SKIP2                                                                
183017     IF MID-IDRADNR-UPPD NOT = ALL '+'                                    
183117       IF MID-IDRADNR-UPPD NOT NUMERIC                                    
183217         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDRADNR-UPPD-ATTR            
183317         MOVE NEJ                     TO INDATA-SW                        
183417         MOVE '002   '                TO MED-IDMFSFEL                     
183517       ELSE                                                               
183617         IF MID-IDRADNR-UPPD = '999'                                      
183717           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDRADNR-UPPD-ATTR            
183817           MOVE NEJ                   TO INDATA-SW                        
183917           MOVE ERR-FEL-RADNR         TO MED-IDMFSFEL                     
184017         ELSE                                                             
184117            MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDRADNR-UPPD-ATTR            
184217            EVALUATE WS-KDARBTYP                                          
184317               WHEN 'ADM'                                                 
184417                  MOVE MID-IDRADNR-UPPD                                   
184517                                      TO W-IDLOPNR-4114                   
184617               WHEN 'RET'                                                 
184717                  MOVE MID-IDRADNR-UPPD                                   
184817                                      TO W-IDLOPNR-4116                   
184917            END-EVALUATE                                                  
185017         END-IF                                                           
185117       END-IF                                                             
185217     ELSE                                                                 
185317       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDRADNR-UPPD-ATTR            
185417       MOVE '003   '                  TO MED-IDMFSFEL                     
185517       MOVE NEJ                       TO INDATA-SW                        
185617     END-IF                                                               
185717     .                                                                    
185817     EJECT                                                                
185917 GAB-KOLLA-KDANMORS SECTION.                                              
186017     SKIP2                                                                
186117     IF MID-KDANMORS-UPPD NOT = ALL '+'                                   
186217       IF MID-KDANMORS-UPPD NOT NUMERIC                                   
186317         IF (MID-KDANMORS-UPPD = 'RT' AND                                 
186417             MID-KDARB-IDPERS-UPPD = ALL '+')                             
186517         OR  MID-KDANMORS-UPPD = 'AD'                                     
186617            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDANMORS-UPPD-ATTR           
186717            EVALUATE WS-KDARBTYP                                          
186817               WHEN 'ADM'                                                 
186917                  MOVE MID-KDANMORS-UPPD                                  
187017                                      TO W-KDANMORS-4114                  
187117               WHEN 'RET'                                                 
187217                  MOVE MID-KDANMORS-UPPD                                  
187317                                      TO W-KDANMORS-4116                  
187417            END-EVALUATE                                                  
187517         ELSE                                                             
187617           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDANMORS-UPPD-ATTR           
187717           MOVE ERR-UPDATE-NOT-OK     TO MED-IDMFSFEL                     
187817           MOVE NEJ                   TO INDATA-SW                        
187917         END-IF                                                           
188017       ELSE                                                               
188117         MOVE MID-KDANMORS-UPPD       TO LKOD-KDANMORS                    
188217         CALL W418OKOD USING LKOD-KDANMORS                                
188317         IF LKOD-FL-GODK-KOD = JA     OR                                  
188417            MID-KDANMORS-UPPD = '97'  OR                                  
188517            MID-KDANMORS-UPPD = HIGH-VALUE                                
188617           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDANMORS-UPPD-ATTR           
188717           EVALUATE WS-KDARBTYP                                           
188817              WHEN 'ADM'                                                  
188917                 MOVE MID-KDANMORS-UPPD                                   
189017                                      TO W-KDANMORS-4114                  
189117              WHEN 'RET'                                                  
189217                 MOVE MID-KDANMORS-UPPD                                   
189317                                      TO W-KDANMORS-4116                  
189417           END-EVALUATE                                                   
189517         ELSE                                                             
189617           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDANMORS-UPPD-ATTR           
189717           MOVE ERR-WRONG-CODE        TO MED-IDMFSFEL                     
189817           CALL WMEDKONV USING MED-WMEDAREA                               
189917           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
190017           MOVE NEJ                   TO INDATA-SW                        
190117         END-IF                                                           
190217       END-IF                                                             
190317     ELSE                                                                 
190417        MOVE HIGH-VALUE               TO W-KDANMORS-4114                  
190517                                         W-KDANMORS-4116                  
190617                                         MID-KDANMORS-UPPD                
190717     END-IF                                                               
190817     .                                                                    
190917     EJECT                                                                
191017 GAC-KOLLA-IDDISTR SECTION.                                               
191117     SKIP2                                                                
191217     IF MID-IDDISTR-FOM-UPPD NOT = ALL '+'                                
191317       IF MID-IDDISTR-FOM-UPPD NOT NUMERIC                                
191417         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-UPPD-ATTR        
191517         MOVE ERR-NOT-NUMERIC         TO MED-IDMFSFEL                     
191617         MOVE NEJ                     TO INDATA-SW                        
191717       ELSE                                                               
191817         IF MID-IDDISTR-TOM-UPPD NOT = ALL '+'                            
191917           IF MID-IDDISTR-TOM-UPPD NUMERIC              AND               
192017             ((MID-IDDISTR-TOM-UPPD > MID-IDDISTR-FOM-UPPD) OR            
192117              (MID-IDDISTR-TOM-UPPD = MID-IDDISTR-FOM-UPPD))              
192217             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-UPPD-ATTR        
192317                                         MOD-IDDISTR-FOM-UPPD-ATTR        
192417             MOVE MID-IDDISTR-FOM-UPPD TO W-IDDISTRF                      
192517             MOVE MID-IDDISTR-TOM-UPPD TO W-IDDISTRT                      
192617           ELSE                                                           
192717             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-UPPD-ATTR        
192817             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-UPPD-ATTR        
192917             MOVE ERR-TOM-LESS-THAN-FOM                                   
193017                                      TO MED-IDMFSFEL                     
193117             MOVE NEJ                 TO INDATA-SW                        
193217           END-IF                                                         
193317         ELSE                                                             
193417           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-FOM-UPPD-ATTR        
193517                                         MOD-IDDISTR-TOM-UPPD-ATTR        
193617         END-IF                                                           
193717       END-IF                                                             
193817     ELSE                                                                 
193917       IF SEGMENT-SAKNAS                                                  
194017         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-UPPD-ATTR        
194117         MOVE ERR-SPECIFY-ONE-CHOICE  TO MED-IDMFSFEL                     
194217         MOVE NEJ                     TO INDATA-SW                        
194317       END-IF                                                             
194417     END-IF                                                               
194517                                                                          
194617     .                                                                    
194717     EJECT                                                                
194817 GAD-KOLLA-IDKUNDNR SECTION.                                              
194917     SKIP2                                                                
195017     IF MID-IDKUNDNR-FOM-UPPD NOT = ALL '+'                               
195117       IF MID-IDKUNDNR-FOM-UPPD NOT NUMERIC                               
195217         MOVE MFS-NUM-FAELT-FEL       TO                                  
195317                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
195417         MOVE ERR-NOT-NUMERIC         TO MED-IDMFSFEL                     
195517         MOVE NEJ                     TO INDATA-SW                        
195617       ELSE                                                               
195717         IF MID-IDKUNDNR-TOM-UPPD NOT = ALL '+'                           
195817           IF MID-IDKUNDNR-TOM-UPPD NUMERIC             AND               
195917             (MID-IDKUNDNR-TOM-UPPD > MID-IDKUNDNR-FOM-UPPD OR            
196017              MID-IDKUNDNR-TOM-UPPD = MID-IDKUNDNR-FOM-UPPD)              
196117             MOVE MFS-NUM-FAELT-RAETT TO                                  
196217                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
196317                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
196417             MOVE MID-IDKUNDNR-FOM-UPPD TO W-IDKUNDNF                     
196517             MOVE MID-IDKUNDNR-TOM-UPPD TO W-IDKUNDNT                     
196617           ELSE                                                           
196717             MOVE MFS-NUM-FAELT-FEL   TO                                  
196817                                      MOD-IDKUNDNR-TOM-UPPD-ATTR          
196917             MOVE MFS-NUM-FAELT-RAETT TO                                  
197017                                      MOD-IDKUNDNR-FOM-UPPD-ATTR          
197117             MOVE ERR-TOM-LESS-THAN-FOM                                   
197217                                      TO MED-IDMFSFEL                     
197317             MOVE NEJ                 TO INDATA-SW                        
197417           END-IF                                                         
197517         ELSE                                                             
197617           MOVE MFS-NUM-FAELT-RAETT   TO                                  
197717                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
197817                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
197917         END-IF                                                           
198017       END-IF                                                             
198117     ELSE                                                                 
198217       IF SEGMENT-SAKNAS                                                  
198317         MOVE MFS-NUM-FAELT-FEL       TO                                  
198417                                        MOD-IDKUNDNR-FOM-UPPD-ATTR        
198517         MOVE ERR-KEY-MISSING         TO MED-IDMFSFEL                     
198617         MOVE NEJ TO INDATA-SW                                            
198717       END-IF                                                             
198817     END-IF                                                               
198917                                                                          
199017     IF MID-IDKUNDNR-TOM-UPPD NOT = ALL '+' AND                           
199117        MID-IDKUNDNR-FOM-UPPD = ALL '+'                                   
199217         MOVE MFS-NUM-FAELT-FEL       TO                                  
199317                                        MOD-IDKUNDNR-TOM-UPPD-ATTR        
199417         MOVE ERR-TOM-LESS-THAN-FOM   TO MED-IDMFSFEL                     
199517         MOVE NEJ TO INDATA-SW                                            
199617     END-IF                                                               
199717                                                                          
199817     .                                                                    
199917     EJECT                                                                
200017 GB-KOLLA-OM-BORTTAG-OK SECTION.                                          
200117     SKIP2                                                                
200217     EVALUATE WS-KDARBTYP                                                 
200317        WHEN 'ADM'                                                        
200417           PERFORM GBA-KOLLA-BORTTAG-ADM                                  
200517        WHEN 'RET'                                                        
200617           PERFORM GBB-KOLLA-BORTTAG-RET                                  
200717     END-EVALUATE                                                         
200817     .                                                                    
200917     EJECT                                                                
201017 GBA-KOLLA-BORTTAG-ADM SECTION.                                           
201117                                                                          
201217     PERFORM IMS-GU-WL411301                                              
201317     IF SEGMENT-FINNS                                                     
201417         PERFORM IMS-GNP-WL411311                                         
201517         IF SEGMENT-FINNS                                                 
201617             PERFORM IMS-GNP-WL411311                                     
201717             IF SEGMENT-FINNS                                             
201817               MOVE JA                TO SW-BORT                          
201917             ELSE                                                         
202017               MOVE NEJ               TO SW-BORT                          
202117               MOVE NEJ               TO INDATA-SW                        
202217               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPPD-ATTR            
202317               MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                     
202417             END-IF                                                       
202517         ELSE                                                             
202617           MOVE NEJ                   TO SW-BORT                          
202717           MOVE NEJ                   TO INDATA-SW                        
202817           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDRADNR-UPPD-ATTR            
202917           MOVE ERR-UPDATE-NOT-OK     TO MED-IDMFSFEL                     
203017         END-IF                                                           
203117     ELSE                                                                 
203217       MOVE NEJ                       TO SW-BORT                          
203317       MOVE NEJ                       TO INDATA-SW                        
203417       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDRADNR-UPPD-ATTR            
203517       MOVE ERR-UPDATE-NOT-OK         TO MED-IDMFSFEL                     
203617     END-IF                                                               
203717     .                                                                    
203817     EJECT                                                                
203917 GBB-KOLLA-BORTTAG-RET SECTION.                                           
204017                                                                          
204117     PERFORM IMS-GU-WL411501                                              
204217     IF SEGMENT-FINNS                                                     
204317         PERFORM IMS-GNP-WL411511                                         
204417         IF SEGMENT-FINNS                                                 
204517             PERFORM IMS-GNP-WL411511                                     
204617             IF SEGMENT-FINNS                                             
204717               MOVE JA                TO SW-BORT                          
204817             ELSE                                                         
204917                                                                          
205017               MOVE NEJ               TO SW-BORT                          
205117               MOVE NEJ               TO INDATA-SW                        
205217               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPPD-ATTR            
205317               MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                     
205417             END-IF                                                       
205517         ELSE                                                             
205617           MOVE NEJ                   TO SW-BORT                          
205717           MOVE NEJ                   TO INDATA-SW                        
205817           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDRADNR-UPPD-ATTR            
205917           MOVE ERR-UPDATE-NOT-OK     TO MED-IDMFSFEL                     
206017         END-IF                                                           
206117     ELSE                                                                 
206217       MOVE NEJ                       TO SW-BORT                          
206317       MOVE NEJ                       TO INDATA-SW                        
206417       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDRADNR-UPPD-ATTR            
206517       MOVE ERR-UPDATE-NOT-OK         TO MED-IDMFSFEL                     
206617     END-IF                                                               
206717     .                                                                    
207017     EJECT                                                                
207500 GC-UPPD-RAD-LOGISK-KOLL SECTION.                                         
207600     SKIP2                                                                
207700     MOVE NEJ                         TO SW-AENDRA                        
207900     EVALUATE WS-KDARBTYP                                                 
208000        WHEN 'ADM'                                                        
208100           PERFORM GCA-UPPD-RAD-LOGISK-KOLL-ADM                           
208200        WHEN 'RET'                                                        
208300           PERFORM GCB-UPPD-RAD-LOGISK-KOLL-RET                           
208400     END-EVALUATE                                                         
208500     .                                                                    
208600     EJECT                                                                
208700 GCA-UPPD-RAD-LOGISK-KOLL-ADM SECTION.                                    
208800                                                                          
208900     MOVE MID-KDANMORS-UPPD           TO W-KDANMORS-4114                  
209000     MOVE MID-IDRADNR-UPPD            TO W-IDLOPNR-4114                   
209100     PERFORM IMS-GU-WL411301                                              
209200     IF SEGMENT-FINNS                                                     
209300       PERFORM  IMS-GHU-WL411311                                          
209400       IF SEGMENT-FINNS                                                   
209500           MOVE JA                    TO SW-AENDRA                        
209600       END-IF                                                             
209900     END-IF                                                               
210000     .                                                                    
210100     EJECT                                                                
210200 GCB-UPPD-RAD-LOGISK-KOLL-RET SECTION.                                    
210300                                                                          
210400     MOVE MID-KDANMORS-UPPD           TO W-KDANMORS-4116                  
210500     MOVE MID-IDRADNR-UPPD            TO W-IDLOPNR-4116                   
210600     PERFORM IMS-GU-WL411501                                              
210700     IF SEGMENT-FINNS                                                     
210800       PERFORM  IMS-GHU-WL411511                                          
210900       IF SEGMENT-FINNS                                                   
211000           MOVE JA                    TO SW-AENDRA                        
211100       END-IF                                                             
211400     END-IF                                                               
211500     .                                                                    
211600     EJECT                                                                
211800 GD-KOLLA-KOD-RADNR SECTION.                                              
211900     SKIP2                                                                
212000     IF MID-KDANMORS-NEXT = ZERO AND                                      
212100        MID-IDRADNR-NEXT = ZERO                                           
212200        IF MID-KDANMORS-UPPD >= MID-KDANMORS-ENTER                        
212300          IF MID-IDRADNR-UPPD >= MID-IDRADNR-ENTER                        
212400             CONTINUE                                                     
212500          ELSE                                                            
212600             MOVE ERR-FEL-RADNR       TO MED-WMEDAREA                     
212700          END-IF                                                          
212800        ELSE                                                              
212900          MOVE ERR-FEL-KOD            TO MED-WMEDAREA                     
213000        END-IF                                                            
213100     ELSE                                                                 
213200*                                                                         
213300        IF MID-KDANMORS-UPPD > MID-KDANMORS-ENTER AND                     
213400           MID-KDANMORS-UPPD < MID-KDANMORS-NEXT                          
213500           CONTINUE                                                       
213600        ELSE                                                              
213700          IF MID-KDANMORS-UPPD = MID-KDANMORS-ENTER                       
213800            IF MID-IDRADNR-UPPD >= MID-IDRADNR-ENTER                      
213900               CONTINUE                                                   
214000            ELSE                                                          
214100               MOVE NEJ               TO INDATA-SW                        
214200               MOVE ERR-FEL-RADNR     TO MED-IDMFSFEL                     
214300               CALL WMEDKONV USING MED-WMEDAREA                           
214400               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
214500            END-IF                                                        
214600          ELSE                                                            
214700            IF MID-KDANMORS-UPPD = MID-KDANMORS-NEXT                      
214800              IF MID-IDRADNR-UPPD <= MID-IDRADNR-NEXT                     
214900                 CONTINUE                                                 
215000              ELSE                                                        
215100                 MOVE NEJ             TO INDATA-SW                        
215200                 MOVE ERR-FEL-RADNR   TO MED-IDMFSFEL                     
215300                 CALL WMEDKONV USING MED-WMEDAREA                         
215400                 MOVE MED-MFSFEL      TO MOD-TEMFSFEL                     
215500              END-IF                                                      
215600            END-IF                                                        
215700          END-IF                                                          
215800        END-IF                                                            
215900     END-IF                                                               
216000     .                                                                    
216100     EJECT                                                                
216200 S01-KOLLA-IDDC SECTION.                                                  
216300                                                                          
216400     IF MID-IDDC-UPPD NOT = ALL '+'                                       
216500        MOVE MID-IDDC-UPPD  TO W-IDDC-B6                                  
216600                               W-IDDC-4118                                
216700        PERFORM IMS-GU-WDB601                                             
216800        IF SEGMENT-FINNS                                                  
216810           MOVE MSGI-IDFTG   TO WS-IDFTG                                  
216900           IF DCS-NDC-NA                                                  
217000             IF (IDFTG-US AND DCS-IDLANDX2 = 'US') OR                     
217100                (IDFTG-CA AND DCS-IDLANDX2 = 'US') OR                     
217110                (IDFTG-CA AND DCS-IDLANDX2 = 'CA') OR                     
217120                (IDFTG-US AND DCS-IDLANDX2 = 'CA')                        
217200                MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPPD-ATTR           
217300             ELSE                                                         
217400                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPPD-ATTR             
217500                MOVE ERR-UPDATE-NOT-OK  TO MED-IDMFSFEL                   
217600                MOVE NEJ TO INDATA-SW                                     
217700             END-IF                                                       
217710           ELSE                                                           
217712             IF DCS-NDC-CN                                                
217713               IF (IDFTG-CN AND DCS-IDLANDX2 = 'CN')                      
217714                                                                          
217715                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPPD-ATTR          
217716               ELSE                                                       
217717                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPPD-ATTR            
217718                 MOVE ERR-UPDATE-NOT-OK  TO MED-IDMFSFEL                  
217719                 MOVE NEJ TO INDATA-SW                                    
217720               END-IF                                                     
217800             ELSE                                                         
217810               IF DCS-NDC-PF                                              
217812               OR DCS-NDC-SA                                              
217813               OR DCS-NDC-OTHERS                                          
217814*               NDC-PF:                                                   
217815                 IF  (DCS-IDLANDX2 = 'IN' AND IDFTG-IN) OR                
217816                     (DCS-IDLANDX2 = 'TH' AND IDFTG-TH) OR                
217817                     (DCS-IDLANDX2 = 'TW' AND IDFTG-TW) OR                
217818                     (DCS-IDLANDX2 = 'KR' AND IDFTG-KR) OR                
217819                     (DCS-IDLANDX2 = 'MY' AND IDFTG-MY) OR                
217820                     (DCS-IDLANDX2 = 'TH' AND IDFTG-TH) OR                
217821                     (DCS-IDLANDX2 = 'TW' AND IDFTG-TW) OR                
217822*               NDC-SA:                                                   
217823                     (DCS-IDLANDX2 = 'BR' AND IDFTG-BR) OR                
217824                     (DCS-IDLANDX2 = 'MX' AND IDFTG-MX) OR                
217825*               NDC-OTHERS:                                               
217826                     (DCS-IDLANDX2 = 'TR' AND IDFTG-TR) OR                
217827                     (DCS-IDLANDX2 = 'ZA' AND IDFTG-ZA) OR                
217828                     (DCS-IDLANDX2 = 'AE' AND IDFTG-AE)                   
217829                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPPD-ATTR        
217830                 ELSE                                                     
217831                   IF IDFTG-PV                                            
217832                     MOVE MFS-ALFA-FAELT-RAETT                            
217833                                            TO MOD-IDDC-UPPD-ATTR         
217834                   ELSE                                                   
217835                     MOVE MFS-ALFA-FAELT-FEL                              
217836                                            TO MOD-IDDC-UPPD-ATTR         
217837                     MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL               
217838                     MOVE NEJ TO INDATA-SW                                
217839                   END-IF                                                 
217840                 END-IF                                                   
217841               ELSE                                                       
217842                 IF IDFTG-PV                                              
217850                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPPD-ATTR        
217860                 ELSE                                                     
218200                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPPD-ATTR          
218300                   MOVE ERR-UPDATE-NOT-OK  TO MED-IDMFSFEL                
218400                   MOVE NEJ TO INDATA-SW                                  
218500                 END-IF                                                   
218510               END-IF                                                     
218600             END-IF                                                       
218610           END-IF                                                         
218700        ELSE                                                              
218800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPPD-ATTR                
218900           MOVE ERR-UPDATE-NOT-OK    TO MED-IDMFSFEL                      
219000           MOVE NEJ TO INDATA-SW                                          
219100        END-IF                                                            
219200     ELSE                                                                 
219300        MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-UPPD-ATTR                
219409        MOVE ERR-INVALID-VALUE       TO MED-IDMFSFEL                      
219500        MOVE NEJ                     TO INDATA-SW                         
219600     END-IF                                                               
219700     .                                                                    
219800     EJECT                                                                
219900 H-UPPDATERA SECTION.                                                     
220000                                                                          
220100     MOVE NEJ TO UPDATE-SW                                                
220200     IF BORTTAG-OK                                                        
220300        EVALUATE WS-KDARBTYP                                              
220400           WHEN 'ADM'                                                     
220500              PERFORM IMS-GU-WL411301                                     
220600              PERFORM IMS-GHU-WL411311                                    
220700                IF SEGMENT-FINNS                                          
220800                  PERFORM IMS-DLET-WL411311                               
220900                  MOVE JA TO UPDATE-SW                                    
221000                END-IF                                                    
221100           WHEN 'RET'                                                     
221200              PERFORM IMS-GU-WL411501                                     
221300              PERFORM IMS-GHU-WL411511                                    
221400                IF SEGMENT-FINNS                                          
221500                  PERFORM IMS-DLET-WL411511                               
221600                  MOVE JA TO UPDATE-SW                                    
221700                END-IF                                                    
221800           WHEN 'REM'                                                     
221900              PERFORM IMS-GHU-WL411711                                    
222000                IF SEGMENT-FINNS                                          
222100                  PERFORM IMS-DLET-WL411711                               
222200                  MOVE JA TO UPDATE-SW                                    
222304                ELSE                                                      
222404                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPPD-ATTR         
222504                  MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                  
222600                END-IF                                                    
222700        END-EVALUATE                                                      
222800     ELSE                                                                 
222900        EVALUATE WS-KDARBTYP                                              
223000           WHEN 'ADM'                                                     
223100              MOVE JA TO UPDATE-SW                                        
223200              PERFORM HA-AENDRA-UPPGIFTER-ADM                             
223300              IF AENDRA-OK                                                
223400                PERFORM IMS-REPL-WL411311                                 
223500              ELSE                                                        
223600                PERFORM IMS-ISRT-WL411311                                 
223700              END-IF                                                      
223800           WHEN 'RET'                                                     
223900              PERFORM HB-AENDRA-UPPGIFTER-RET                             
224000              MOVE JA TO UPDATE-SW                                        
224100              IF AENDRA-OK                                                
224200                PERFORM IMS-REPL-WL411511                                 
224300              ELSE                                                        
224400                PERFORM IMS-ISRT-WL411511                                 
224500              END-IF                                                      
224600           WHEN 'REM'                                                     
224809              IF NYTT                                                     
224909                PERFORM HC-AENDRA-UPPGIFTER-REM                           
225009                PERFORM IMS-ISRT-WL411711                                 
225010                PERFORM UNTIL SEGMENT-FINNS                               
225020                  ADD +1 TO 4118-IDLOPNR                                  
225030                            W-IDLOPNR-4118                                
225040                  PERFORM IMS-ISRT-WL411711                               
225050                END-PERFORM                                               
225509                MOVE JA TO UPDATE-SW                                      
226400              ELSE                                                        
226500                PERFORM IMS-GHU-WL411711                                  
226800                IF SEGMENT-FINNS                                          
226909                  PERFORM HC-AENDRA-UPPGIFTER-REM                         
227000                  PERFORM IMS-REPL-WL411711                               
227109                  MOVE JA TO UPDATE-SW                                    
227200                ELSE                                                      
227309                  MOVE ERR-DUPLICATE-RECORD TO MED-IDMFSFEL               
227409                  CALL WMEDKONV USING MED-WMEDAREA                        
227509                  MOVE MED-MFSFEL      TO MOD-TEMFSFEL                    
227700                END-IF                                                    
227800              END-IF                                                      
227900        END-EVALUATE                                                      
228000     END-IF                                                               
228100                                                                          
228200     IF UPDATE-SUCCESS                                                    
228300       MOVE INF-UPDATE-DONE             TO MED-IDMFSINF                   
228400       CALL WMEDKONV USING MED-WMEDAREA                                   
228500       MOVE MED-MFSINF                  TO MOD-TEMFSINF                   
228600       PERFORM MFS-FORM-ATTR                                              
228700       PERFORM MFS-RENSA-FAELT-IN                                         
228800     END-IF                                                               
228900     .                                                                    
229000     EJECT                                                                
229100 HA-AENDRA-UPPGIFTER-ADM SECTION.                                         
229200     SKIP2                                                                
229300     MOVE W-KDANMORS-4114             TO 4114-KDANMORS                    
229400     MOVE W-IDLOPNR-4114              TO 4114-IDLOPNR                     
229500     MOVE W-PRARTNTO                  TO 4114-PRARTNTO                    
229600                                                                          
229700     IF MID-IDDISTR-FOM-UPPD    NOT = ALL '+'                             
229800       MOVE MID-IDDISTR-FOM-UPPD      TO 4114-IDDISTR-FOM                 
229900       IF MID-IDDISTR-TOM-UPPD = ALL '+'                                  
230000           MOVE MID-IDDISTR-FOM-UPPD  TO 4114-IDDISTR-TOM                 
230100       ELSE                                                               
230200           MOVE MID-IDDISTR-TOM-UPPD  TO 4114-IDDISTR-TOM                 
230300       END-IF                                                             
230400     END-IF                                                               
230500                                                                          
230600     IF MID-IDKUNDNR-FOM-UPPD   NOT = ALL '+'                             
230700       MOVE MID-IDKUNDNR-FOM-UPPD     TO 4114-IDKUNDNR-FOM                
230800       IF MID-IDKUNDNR-TOM-UPPD = ALL '+'                                 
230900           MOVE MID-IDKUNDNR-FOM-UPPD TO 4114-IDKUNDNR-TOM                
231000       ELSE                                                               
231100           MOVE MID-IDKUNDNR-TOM-UPPD TO 4114-IDKUNDNR-TOM                
231200       END-IF                                                             
231300     END-IF                                                               
231400                                                                          
231500     IF MID-KDARB-IDPERS-UPPD = ALL '+'                                   
231600       IF AENDRA-OK                                                       
231700         CONTINUE                                                         
231800       ELSE                                                               
231900         MOVE SPACE                   TO 4114-KDARBTYP-ADM                
232000         MOVE ZERO                    TO 4114-IDPERSON-ADM                
232100       END-IF                                                             
232200     ELSE                                                                 
232300       MOVE MID-KDARB-IDPERS-UPPD     TO WS-KDARBTYP-IDPERSON             
232400       MOVE WS-SPAR-KDARBTYP          TO 4114-KDARBTYP-ADM                
232500       MOVE WS-SPAR-IDPERSON          TO 4114-IDPERSON-ADM                
232600     END-IF                                                               
232700     .                                                                    
232800     EJECT                                                                
232900 HB-AENDRA-UPPGIFTER-RET SECTION.                                         
233000     SKIP2                                                                
233100     MOVE W-KDANMORS-4116             TO 4116-KDANMORS                    
233200     MOVE W-IDLOPNR-4116              TO 4116-IDLOPNR                     
233300     MOVE MID-IDDC-UPPD               TO 4116-IDDC                        
233400     MOVE W-PRARTNTO                  TO 4116-PRARTNTO                    
233500                                                                          
233600     IF MID-IDDISTR-FOM-UPPD    NOT = ALL '+'                             
233700       MOVE MID-IDDISTR-FOM-UPPD      TO 4116-IDDISTR-FOM                 
233800       IF MID-IDDISTR-TOM-UPPD = ALL '+'                                  
233900           MOVE MID-IDDISTR-FOM-UPPD  TO 4116-IDDISTR-TOM                 
234000       ELSE                                                               
234100           MOVE MID-IDDISTR-TOM-UPPD  TO 4116-IDDISTR-TOM                 
234200       END-IF                                                             
234300     END-IF                                                               
234400                                                                          
234500     IF MID-IDKUNDNR-FOM-UPPD   NOT = ALL '+'                             
234600       MOVE MID-IDKUNDNR-FOM-UPPD     TO 4116-IDKUNDNR-FOM                
234700       IF MID-IDKUNDNR-TOM-UPPD = ALL '+'                                 
234800           MOVE MID-IDKUNDNR-FOM-UPPD TO 4116-IDKUNDNR-TOM                
234900       ELSE                                                               
235000           MOVE MID-IDKUNDNR-TOM-UPPD TO 4116-IDKUNDNR-TOM                
235100       END-IF                                                             
235200     END-IF                                                               
235300                                                                          
235400     IF MID-KDARB-IDPERS-UPPD = ALL '+'                                   
235500       IF AENDRA-OK                                                       
235600         CONTINUE                                                         
235700       ELSE                                                               
235800         MOVE SPACE                   TO 4116-KDARBTYP-RET                
235900         MOVE ZERO                    TO 4116-IDPERSON-RET                
236000       END-IF                                                             
236100     ELSE                                                                 
236200       MOVE MID-KDARB-IDPERS-UPPD     TO WS-KDARBTYP-IDPERSON             
236300       MOVE WS-SPAR-KDARBTYP          TO 4116-KDARBTYP-RET                
236400       MOVE WS-SPAR-IDPERSON          TO 4116-IDPERSON-RET                
236500     END-IF                                                               
236600     .                                                                    
236700     EJECT                                                                
236800 HC-AENDRA-UPPGIFTER-REM SECTION.                                         
237400                                                                          
237515     IF MID-IDRADNR-UPPD        NOT = ALL '+'                             
237615       MOVE MID-IDRADNR-UPPD          TO 4118-IDLOPNR                     
237709     ELSE                                                                 
237710       MOVE +2                        TO 4118-IDLOPNR                     
237720                                         W-IDLOPNR-4118                   
239409     END-IF                                                               
239509                                                                          
239510     MOVE W-KDANMORS-4118             TO 4118-KDANMORS                    
239520     MOVE W-IDDC-4118                 TO 4118-IDDC                        
239530                                                                          
239609     IF MID-KDORDKL-UPPD        NOT = ALL '+'                             
239709       MOVE MID-KDORDKL-UPPD          TO 4118-KDORDKL                     
239809     END-IF                                                               
239909                                                                          
240009     IF MID-ADLAGOMR-UPPD       NOT = ALL '+'                             
240109       MOVE MID-ADLAGOMR-UPPD         TO 4118-ADLAGOMR                    
240209     END-IF                                                               
240309                                                                          
240409     IF MID-IDDISTR-FOM-UPPD    NOT = ALL '+'                             
240509       MOVE MID-IDDISTR-FOM-UPPD      TO 4118-IDDISTR-FOM                 
240609       IF MID-IDDISTR-TOM-UPPD = ALL '+'                                  
240709           MOVE MID-IDDISTR-FOM-UPPD  TO 4118-IDDISTR-TOM                 
240809       ELSE                                                               
240909           MOVE MID-IDDISTR-TOM-UPPD  TO 4118-IDDISTR-TOM                 
241009       END-IF                                                             
241109     END-IF                                                               
241209                                                                          
241309     IF MID-IDKUNDNR-FOM-UPPD   NOT = ALL '+'                             
241409       MOVE MID-IDKUNDNR-FOM-UPPD     TO 4118-IDKUNDNR-FOM                
241509       IF MID-IDKUNDNR-TOM-UPPD = ALL '+'                                 
241609           MOVE MID-IDKUNDNR-FOM-UPPD TO 4118-IDKUNDNR-TOM                
241709       ELSE                                                               
241809           MOVE MID-IDKUNDNR-TOM-UPPD TO 4118-IDKUNDNR-TOM                
241909       END-IF                                                             
242009     END-IF                                                               
242109                                                                          
242209     IF MID-KDARB-IDPERS-UPPD NOT = ALL '+'                               
242309       MOVE MID-KDARB-IDPERS-UPPD     TO WS-KDARBTYP-IDPERSON             
242409       MOVE WS-SPAR-KDARBTYP          TO 4118-KDARBTYP-REM                
242509       MOVE WS-SPAR-IDPERSON          TO 4118-IDPERSON-REM                
242609     END-IF                                                               
242709     MOVE ZERO                        TO 4118-PRARTNTO                    
242809     .                                                                    
242909     EJECT                                                                
243009 MFS-RENSA-FAELT-UT SECTION.                                              
243109                                                                          
243209*    --- ALLA UTDATA-FÄLT                                                 
243309*    --- INKL. BLÄDDRINGSNYCKLAR                                          
243409     MOVE MFS-RENSA-FAELT             TO MOD-IDRADNR-ENTER                
243509                                         MOD-IDRADNR-NEXT                 
243609                                         MOD-KDANMORS-ENTER               
243709                                         MOD-KDANMORS-NEXT                
243809                                         MOD-IDDC-ENTER                   
243909                                         MOD-IDDC-NEXT                    
244009                                         MOD-IDRADNR-RAD1                 
244109                                         MOD-KDANMORS-RAD1                
244209                                         MOD-KDORDKL-RAD1                 
244309                                         MOD-ADLAGOMR-RAD1                
244409                                         MOD-IDDISTR-FOM-RAD1             
244509                                         MOD-IDDISTR-TOM-RAD1             
244609                                         MOD-IDKUNDNR-FOM-RAD1            
244709                                         MOD-IDKUNDNR-TOM-RAD1            
244809                                         MOD-ARB-PERS-RAD1                
244909                                         MOD-IDMAIL30-RAD1                
245009                                                                          
245109     MOVE +1                          TO INDX                             
245209     PERFORM UNTIL INDX > MAX-INDX                                        
245309         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
245409         ADD +1                       TO INDX                             
245509     END-PERFORM                                                          
245609     EJECT                                                                
245709     .                                                                    
245809                                                                          
245909 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
246009                                                                          
246109*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
246209     MOVE MFS-RENSA-FAELT             TO MOD-IDRADNR       (INDX)         
246309                                         MOD-KDANMORS      (INDX)         
246409                                         MOD-IDDC          (INDX)         
246410                                         MOD-KDORDKL       (INDX)         
246509                                         MOD-ADLAGOMR      (INDX)         
246609                                         MOD-IDDISTR-FOM   (INDX)         
246709                                         MOD-IDDISTR-TOM   (INDX)         
246809                                         MOD-IDKUNDNR-FOM  (INDX)         
246909                                         MOD-IDKUNDNR-TOM  (INDX)         
247009                                         MOD-KDARBTYP      (INDX)         
247109                                         MOD-IDPERSON      (INDX)         
247209                                         MOD-IDMAIL30      (INDX)         
247309     .                                                                    
247409                                                                          
247509 MFS-RENSA-RAD SECTION.                                                   
247609                                                                          
247709*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
247809     MOVE MFS-RENSA-FAELT             TO MOD-IDRADNR       (INDX)         
247909                                         MOD-KDANMORS      (INDX)         
248009                                         MOD-IDDC          (INDX)         
248109     .                                                                    
248209                                                                          
248309 MFS-RENSA-FAELT-IN SECTION.                                              
248409                                                                          
248509*    --- ALLA INDATA-FÄLT                                                 
248609     MOVE MFS-RENSA-FAELT             TO MOD-KDARBTYP-IN                  
248709                                         MOD-KDANMORS-IN                  
248809                                         MOD-IDDISTR-IN                   
248909                                         MOD-IDDC-IN                      
249009                                         MOD-IDRADNR-UPPD                 
249109                                         MOD-KDANMORS-UPPD                
249209                                         MOD-IDDC-UPPD                    
249309                                         MOD-KDORDKL-UPPD                 
249409                                         MOD-ADLAGOMR-UPPD                
249509                                         MOD-IDDISTR-FOM-UPPD             
249609                                         MOD-IDDISTR-TOM-UPPD             
249709                                         MOD-IDKUNDNR-FOM-UPPD            
249809                                         MOD-IDKUNDNR-TOM-UPPD            
249909                                         MOD-KDARB-IDPERS-UPD             
250009                                         MOD-FLBORT-UPPD                  
250109     .                                                                    
250209     EJECT                                                                
250309 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
250409                                                                          
250509*    --- ALLA UTDATA-FÄLT                                                 
250609*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
250709     MOVE MFS-ROER-EJ-FAELT           TO MOD-ANSVARIG-RAD1                
250809                                         MOD-IDRADNR-RAD1                 
250909                                         MOD-KDANMORS-RAD1                
251009                                         MOD-IDDC-RAD1                    
251010                                         MOD-KDORDKL-RAD1                 
251109                                         MOD-ADLAGOMR-RAD1                
251209                                         MOD-IDDISTR-FOM-RAD1             
251309                                         MOD-IDDISTR-TOM-RAD1             
251409                                         MOD-IDKUNDNR-FOM-RAD1            
251509                                         MOD-IDKUNDNR-TOM-RAD1            
251609                                         MOD-ARB-PERS-RAD1                
251709                                         MOD-IDMAIL30-RAD1                
251809                                         MOD-IDRADNR-ENTER                
251909                                         MOD-IDRADNR-NEXT                 
252009                                         MOD-KDANMORS-ENTER               
252109                                         MOD-KDANMORS-NEXT                
252209                                         MOD-IDDC-ENTER                   
252309                                         MOD-IDDC-NEXT                    
252409                                                                          
252509     MOVE +1                          TO INDX                             
252609     PERFORM UNTIL INDX > MAX-INDX                                        
252709       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
252809       ADD +1                         TO INDX                             
252909     END-PERFORM                                                          
253009     .                                                                    
253109                                                                          
253209 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
253309                                                                          
253409*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
253509     MOVE MFS-ROER-EJ-FAELT           TO MOD-IDRADNR       (INDX)         
253609                                         MOD-KDANMORS      (INDX)         
253709                                         MOD-IDDC          (INDX)         
253710                                         MOD-KDORDKL       (INDX)         
253809                                         MOD-ADLAGOMR      (INDX)         
253909                                         MOD-IDDISTR-FOM   (INDX)         
254009                                         MOD-IDDISTR-TOM   (INDX)         
254109                                         MOD-IDKUNDNR-FOM  (INDX)         
254209                                         MOD-IDKUNDNR-TOM  (INDX)         
254309                                         MOD-KDARBTYP      (INDX)         
254409                                         MOD-IDPERSON      (INDX)         
254509                                         MOD-IDMAIL30      (INDX)         
254609      .                                                                   
254709                                                                          
254809 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
254909                                                                          
255009*    --- ALLA INDATA-FÄLT                                                 
255109     MOVE MFS-ROER-EJ-FAELT           TO MOD-KDARBTYP-IN                  
255209                                         MOD-KDANMORS-IN                  
255309                                         MOD-IDDC-IN                      
255409                                         MOD-IDRADNR-UPPD                 
255509                                         MOD-KDANMORS-UPPD                
255609                                         MOD-KDORDKL-UPPD                 
255709                                         MOD-ADLAGOMR-UPPD                
255809                                         MOD-IDDC-UPPD                    
255909                                         MOD-IDDISTR-FOM-UPPD             
256009                                         MOD-IDDISTR-TOM-UPPD             
256109                                         MOD-IDKUNDNR-FOM-UPPD            
256209                                         MOD-IDKUNDNR-TOM-UPPD            
256309                                         MOD-KDARB-IDPERS-UPD             
256409                                         MOD-FLBORT-UPPD                  
256509     .                                                                    
256609     EJECT                                                                
256709 MFS-FORM-ATTR SECTION.                                                   
256809                                                                          
256909*    --- ALLA INDATA-FÄLT                                                 
257009     MOVE MFS-FORMATETS-ATTR     TO MOD-IDRADNR-UPPD-ATTR                 
257109                                    MOD-KDANMORS-UPPD-ATTR                
257209                                    MOD-IDDC-UPPD-ATTR                    
257309                                    MOD-KDORDKL-UPPD-ATTR                 
257409                                    MOD-ADLAGOMR-UPPD-ATTR                
257509                                    MOD-IDDISTR-FOM-UPPD-ATTR             
257609                                    MOD-IDDISTR-TOM-UPPD-ATTR             
257709                                    MOD-IDKUNDNR-FOM-UPPD-ATTR            
257809                                    MOD-IDKUNDNR-TOM-UPPD-ATTR            
257909                                    MOD-KDARB-IDPERS-UPD-ATTR             
258009                                    MOD-FLBORT-UPPD-ATTR                  
258109     .                                                                    
258209     EJECT                                                                
258309                                                                          
258409* --- IMS SEKTIONER ---                                                   
258509                                                                          
258609 IMS-GET-MSG SECTION.                                                     
258709                                                                          
258809     MOVE '  QC' TO GODK-STATUSKODER                                      
258909     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
259009     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259109     PERFORM IMS-STATUSKONTROLL                                           
259209     .                                                                    
259309                                                                          
259409 IMS-INSERT-MSG SECTION.                                                  
259509                                                                          
259609     IF MSGI-IDLAND-SPR = 'GB'                                            
259709       MOVE 'N' TO MFS-KDHUVOMR                                           
259809     END-IF                                                               
259909     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
260009     MOVE SPACE TO GODK-STATUSKODER                                       
260109     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
260209     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
260309     PERFORM IMS-STATUSKONTROLL                                           
260409     .                                                                    
260509     EJECT                                                                
260609 IMS-GU-WL411301 SECTION.                                                 
260709                                                                          
260809     STRING 'WL411301(WDGXKEY  =' W-WDGXKEY-ROT-4113-X ')'                
260909          DELIMITED BY SIZE INTO SSA1                                     
261009     MOVE '  ' TO GODK-STATUSKODER                                        
261109     CALL CBLTDLI USING GU 4113-PCB DLI-IO-AREA-4113 SSA1                 
261209     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
261309     PERFORM IMS-STATUSKONTROLL                                           
261409     .                                                                    
261509     SKIP3                                                                
261609 IMS-GNP-WL411311 SECTION.                                                
261709                                                                          
261809     STRING 'WL411311(KEY4114 >=' W-KEY4114-MIN-X                         
261909                    '&KEY4114 <=' W-KEY4114-MAX-X                         
262009                    '&IDDISTRF<=' W-IDDISTR-FOM-X                         
262109                    '&IDDISTRT>=' W-IDDISTR-TOM-X ')'                     
262209          DELIMITED BY SIZE INTO SSA1                                     
262309     MOVE '  GBGE' TO GODK-STATUSKODER                                    
262409     CALL CBLTDLI USING GNP 4113-PCB DLI-IO-AREA-4114 SSA1                
262509     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
262609     PERFORM IMS-STATUSKONTROLL                                           
262709     .                                                                    
262809     EJECT                                                                
262909 IMS-GU-WL411311 SECTION.                                                 
263009                                                                          
263109     STRING 'WL411301(WDGXKEY  =' W-WDGXKEY-ROT-4113-X ')'                
263209          DELIMITED BY SIZE INTO SSA1                                     
263309     STRING 'WL411311(KEY4114  =' W-KEY4114-X ')'                         
263409          DELIMITED BY SIZE INTO SSA2                                     
263509     MOVE '  GE' TO GODK-STATUSKODER                                      
263609     CALL CBLTDLI USING GU 4113-PCB DLI-IO-AREA-4114 SSA1 SSA2            
263709     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
263809     PERFORM IMS-STATUSKONTROLL                                           
263909     .                                                                    
264009     EJECT                                                                
264109 IMS-GHU-WL411311 SECTION.                                                
264209                                                                          
264309     STRING 'WL411311(KEY4114  =' W-KEY4114-X ')'                         
264409          DELIMITED BY SIZE INTO SSA1                                     
264509     MOVE '  GE' TO GODK-STATUSKODER                                      
264609     CALL CBLTDLI USING GHU 4113-PCB DLI-IO-AREA-4114 SSA1                
264709     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
264809     PERFORM IMS-STATUSKONTROLL                                           
264909     .                                                                    
265009     EJECT                                                                
265109 IMS-ISRT-WL411311 SECTION.                                               
265209                                                                          
265309     STRING 'WL411301(WDGXKEY  =' W-WDGXKEY-ROT-4113-X ')'                
265409          DELIMITED BY SIZE INTO SSA1                                     
265509     STRING 'WL411311 '                                                   
265609          DELIMITED BY SIZE INTO SSA2                                     
265709     MOVE '  ' TO GODK-STATUSKODER                                        
265809     CALL CBLTDLI USING ISRT 4113-PCB DLI-IO-AREA-4114 SSA1 SSA2          
265909     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
266009     PERFORM IMS-STATUSKONTROLL                                           
266109     .                                                                    
266209     EJECT                                                                
266309 IMS-REPL-WL411311 SECTION.                                               
266409                                                                          
266509     MOVE '  ' TO GODK-STATUSKODER                                        
266609     CALL CBLTDLI USING REPL 4113-PCB DLI-IO-AREA-4114                    
266709     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
266809     PERFORM IMS-STATUSKONTROLL                                           
266909     .                                                                    
267009     EJECT                                                                
267109 IMS-DLET-WL411311 SECTION.                                               
267209                                                                          
267309     MOVE '  ' TO GODK-STATUSKODER                                        
267409     CALL CBLTDLI USING DLET 4113-PCB DLI-IO-AREA-4114                    
267509     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
267609     PERFORM IMS-STATUSKONTROLL                                           
267709     .                                                                    
267809                                                                          
267909 IMS-GU-WL411501 SECTION.                                                 
268009                                                                          
268109     STRING 'WL411501(WDGXKEY  =' W-WDGXKEY-ROT-4115-X ')'                
268209          DELIMITED BY SIZE INTO SSA1                                     
268309     MOVE '  ' TO GODK-STATUSKODER                                        
268409     CALL CBLTDLI USING GU 4115-PCB DLI-IO-AREA-4115 SSA1                 
268509     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
268609     PERFORM IMS-STATUSKONTROLL                                           
268709     .                                                                    
268809     SKIP3                                                                
268909 IMS-GNP-WL411511 SECTION.                                                
269009                                                                          
269109     STRING 'WL411511(KEY4116 >=' W-KEY4116-MIN-X                         
269209                    '&KEY4116 <=' W-KEY4116-MAX-X                         
269309                    '&IDDISTRF<=' W-IDDISTR-FOM-X                         
269409                    '&IDDISTRT>=' W-IDDISTR-TOM-X ')'                     
269509          DELIMITED BY SIZE INTO SSA1                                     
269609     MOVE '  GBGE' TO GODK-STATUSKODER                                    
269709     CALL CBLTDLI USING GNP 4115-PCB DLI-IO-AREA-4116 SSA1                
269809     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
269909     PERFORM IMS-STATUSKONTROLL                                           
270009     .                                                                    
270109     EJECT                                                                
270209 IMS-GU-WL411511 SECTION.                                                 
270309                                                                          
270409     STRING 'WL411501(WDGXKEY  =' W-WDGXKEY-ROT-4115-X ')'                
270509          DELIMITED BY SIZE INTO SSA1                                     
270609     STRING 'WL411511(KEY4116  =' W-KEY4116-X ')'                         
270709          DELIMITED BY SIZE INTO SSA2                                     
270809     MOVE '  GE' TO GODK-STATUSKODER                                      
270909     CALL CBLTDLI USING GU 4115-PCB DLI-IO-AREA-4116 SSA1 SSA2            
271009     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
271109     PERFORM IMS-STATUSKONTROLL                                           
271209     .                                                                    
271309     EJECT                                                                
271409 IMS-GHU-WL411511 SECTION.                                                
271509                                                                          
271609     STRING 'WL411511(KEY4116  =' W-KEY4116-X ')'                         
271709          DELIMITED BY SIZE INTO SSA1                                     
271809     MOVE '  GE' TO GODK-STATUSKODER                                      
271909     CALL CBLTDLI USING GHU 4115-PCB DLI-IO-AREA-4116 SSA1                
272009     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
272109     PERFORM IMS-STATUSKONTROLL                                           
272209     .                                                                    
272309     EJECT                                                                
272409 IMS-ISRT-WL411511 SECTION.                                               
272509                                                                          
272609     STRING 'WL411501(WDGXKEY  =' W-WDGXKEY-ROT-4115-X ')'                
272709          DELIMITED BY SIZE INTO SSA1                                     
272809     STRING 'WL411511 '                                                   
272909          DELIMITED BY SIZE INTO SSA2                                     
273009     MOVE '  ' TO GODK-STATUSKODER                                        
273109     CALL CBLTDLI USING ISRT 4115-PCB DLI-IO-AREA-4116 SSA1 SSA2          
273209     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
273309     PERFORM IMS-STATUSKONTROLL                                           
273409     .                                                                    
273509     EJECT                                                                
273609 IMS-REPL-WL411511 SECTION.                                               
273709                                                                          
273809     MOVE '  ' TO GODK-STATUSKODER                                        
273909     CALL CBLTDLI USING REPL 4115-PCB DLI-IO-AREA-4116                    
274009     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
274109     PERFORM IMS-STATUSKONTROLL                                           
274209     .                                                                    
274309     EJECT                                                                
274409 IMS-DLET-WL411511 SECTION.                                               
274509                                                                          
274609     MOVE '  ' TO GODK-STATUSKODER                                        
274709     CALL CBLTDLI USING DLET 4115-PCB DLI-IO-AREA-4116                    
274809     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
274909     PERFORM IMS-STATUSKONTROLL                                           
275009     .                                                                    
275109                                                                          
275209 IMS-GU-WL411701 SECTION.                                                 
275309                                                                          
275409     STRING 'WL411701(WDGXKEY  =' W-WDGXKEY-ROT-4117-X ')'                
275509          DELIMITED BY SIZE INTO SSA1                                     
275609     MOVE '  ' TO GODK-STATUSKODER                                        
275709     CALL CBLTDLI USING GU 4117-PCB DLI-IO-AREA-4117 SSA1                 
275809     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
275909     PERFORM IMS-STATUSKONTROLL                                           
276009     .                                                                    
276109     SKIP3                                                                
276209 IMS-GNP-WL411711 SECTION.                                                
276309                                                                          
276409     STRING 'WL411711(KEY4118 >=' W-KEY4118-MIN-X                         
276509                    '&KEY4118 <=' W-KEY4118-MAX-X                         
276609                    '&IDDISTRF<=' W-IDDISTR-FOM-X                         
276709                    '&IDDISTRT>=' W-IDDISTR-TOM-X                         
276809                    '&IDDC    >=' W-IDDC-MIN-X                            
276909                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
277009          DELIMITED BY SIZE INTO SSA1                                     
277109     MOVE '  GBGE' TO GODK-STATUSKODER                                    
277209     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
277309     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
277409     PERFORM IMS-STATUSKONTROLL                                           
277509     .                                                                    
277609     EJECT                                                                
277700 IMS-GHU-WL411711 SECTION.                                                
277800                                                                          
277900     STRING 'WL411701(WDGXKEY  =' W-WDGXKEY-ROT-4117-X ')'                
278000          DELIMITED BY SIZE INTO SSA1                                     
278100     STRING 'WL411711(KEY4118  =' W-KEY4118-X ')'                         
278200          DELIMITED BY SIZE INTO SSA2                                     
278300     MOVE '  GE' TO GODK-STATUSKODER                                      
278400     CALL CBLTDLI USING GHU 4117-PCB DLI-IO-AREA-4118 SSA1 SSA2           
278500     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
278600     PERFORM IMS-STATUSKONTROLL                                           
278700     .                                                                    
278800     EJECT                                                                
278909 IMS-GNP-WL411711-KOD-DC SECTION.                                         
279000                                                                          
279109     STRING 'WL411711(KDANMORS =' W-KDANMORS-4118                         
279110                    '&IDDC     =' W-IDDC-B6-X ')'                         
279200          DELIMITED BY SIZE INTO SSA1                                     
279300     MOVE '  GE' TO GODK-STATUSKODER                                      
279400     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
279500     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
279600     PERFORM IMS-STATUSKONTROLL                                           
279700     .                                                                    
279800                                                                          
279900 IMS-ISRT-WL411711 SECTION.                                               
280000                                                                          
280100     STRING 'WL411701(WDGXKEY  =' W-WDGXKEY-ROT-4117-X ')'                
280200          DELIMITED BY SIZE INTO SSA1                                     
280300     STRING 'WL411711 '                                                   
280400          DELIMITED BY SIZE INTO SSA2                                     
280500     MOVE '  II' TO GODK-STATUSKODER                                      
280600     CALL CBLTDLI USING ISRT 4117-PCB DLI-IO-AREA-4118 SSA1 SSA2          
280700     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
280800     PERFORM IMS-STATUSKONTROLL                                           
280900     .                                                                    
281000     EJECT                                                                
281100 IMS-REPL-WL411711 SECTION.                                               
281200                                                                          
281300     MOVE '  ' TO GODK-STATUSKODER                                        
281400     CALL CBLTDLI USING REPL 4117-PCB DLI-IO-AREA-4118                    
281500     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
281600     PERFORM IMS-STATUSKONTROLL                                           
281700     .                                                                    
281800     EJECT                                                                
281900 IMS-DLET-WL411711 SECTION.                                               
282000                                                                          
282100     MOVE '  ' TO GODK-STATUSKODER                                        
282200     CALL CBLTDLI USING DLET 4117-PCB DLI-IO-AREA-4118                    
282300     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
282400     PERFORM IMS-STATUSKONTROLL                                           
282500     .                                                                    
282600                                                                          
282700 IMS-GU-WDP311 SECTION.                                                   
282800                                                                          
282900     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
283000          DELIMITED BY SIZE INTO SSA1                                     
283100     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
283200          DELIMITED BY SIZE INTO SSA2                                     
283300     MOVE '  GE' TO GODK-STATUSKODER                                      
283400     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-AREA-P311 SSA1 SSA2            
283500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
283600     PERFORM IMS-STATUSKONTROLL                                           
283700     .                                                                    
283800     EJECT                                                                
286100 IMS-GU-WDB601    SECTION.                                                
286200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
286300          DELIMITED BY SIZE INTO SSA1                                     
286400     MOVE '  GE' TO GODK-STATUSKODER                                      
286500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
286600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
286700     PERFORM IMS-STATUSKONTROLL                                           
286800     IF SEGMENT-SAKNAS                                                    
286900        MOVE SPACE TO DCS-KDDC                                            
287000     END-IF                                                               
287100     .                                                                    
287200     EJECT                                                                
287300 IMS-STATUSKONTROLL SECTION.                                              
287400                                                                          
287500     SET STATUS-IX TO 1                                                   
287600     SEARCH GODK-STATUS                                                   
287700       AT END                                                             
287800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
287900         DELIMITED BY SIZE INTO FELTEXT                                   
288000         CALL FELLOG                                                      
288100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
288200         CONTINUE                                                         
289000     END-SEARCH                                                           
290000     .                                                                    
