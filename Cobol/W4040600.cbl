000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040600.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   08/02/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DEFINE CASE ID FOR WEB PRINT                                     
000900*        BILDEN SKALL GE STARTVÄRDEN FÖR KOLLI FÖR                        
001000*        RESP DC OCH PRC                                                  
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDR1                                       
001300*        PROGRAMMET LÄSER      WDK5                                       
001400*        PROGRAMMET LÄSER/UPPD WDB6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T406                                              
001800*                     W4T406U                                             
001900*        MID:         W4I40601                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O40601                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4040600'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FILLER                      PIC X(08) VALUE 'FELTEXT:'.              
003310 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400 77  CURRENT-SECTION             PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004010 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +39   COMP SYNC.        
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004500     88  NYCKLAR-OK                          VALUE 'J'.                   
004600     88  NYCKLAR-FEL                         VALUE 'N'.                   
004700                                                                          
004800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004900     88  INDATA-OK                           VALUE 'J'.                   
005000     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  UPPD-SW                     PIC X       VALUE ' '.                   
005300     88  INSRT                               VALUE 'I'.                   
005400     88  NEW                                 VALUE 'N'.                   
005500     88  ANDRA                               VALUE 'C'.                   
005600     88  EDIT                                VALUE 'E'.                   
005609     88  DLETE                               VALUE 'D'.                   
005709                                                                          
005809 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005909     88  EGEN-MID                            VALUE '4406'.                
006009     88  GODK-MID                            VALUE '4401' '4402'          
006109                                                   '4403' '4404'          
006209                                                   '4405' '4406'          
006309                                                   '4407' '4408'          
006409                                                   '4409'.                
006509     88  HELP-MID                            VALUE '0551'.                
006609     EJECT                                                                
006709*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006809 01  GENERELLA-SUBPROGRAM.                                                
006909     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007009     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007109     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007209     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007309     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007409     EJECT                                                                
007509*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007609*01 -COPY WMEDAREA                                                        
007709     SKIP3                                                                
007809 01  MESSAGE-CODES.                                                       
007909     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008009     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008010     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008020     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008109     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008110     03  ERR-WRONG-DC            PIC X(3)    VALUE '026'.                 
008120     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008209     EJECT                                                                
008309*01  -COPY WDATAREA                                                       
008409     EJECT                                                                
008509*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008609*                                                                         
008709 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008809     SKIP3                                                                
008909*01 -COPY WMSGINIT                                                        
009009     EJECT                                                                
009109*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009209*                                                                         
009309 01  SPAR-AREA.                                                           
009409     03  SPAR-IDTRANS           PIC X(4)    VALUE '4406'.                 
009509     03  SPAR-IDDC-ENTER        PIC X(2).                                 
009609     03  SPAR-IDDC-NEXT         PIC X(2).                                 
009709     03  SPAR-IDPRC-ENTER       PIC X(4).                                 
009809     03  SPAR-IDPRC-NEXT        PIC X(4).                                 
009909     EJECT                                                                
010009*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010109*                                                                         
010209 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010309     SKIP3                                                                
010409*01  MID -COPY W4I40601                                                   
010509     EJECT                                                                
010609 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010709     SKIP3                                                                
010809*01  -COPY WMSGAREA                                                       
010909                                                                          
011009     03  MOD REDEFINES MSG-AREA.                                          
011109*      05  -COPY W4O40601                                                 
011209     EJECT                                                                
011309 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011409     SKIP3                                                                
011509*01  -COPY WMFSAREA                                                       
011609     EJECT                                                                
011709*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011809*                                                                         
011909 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012009     SKIP3                                                                
012109 01  NYCKLAR-TILL-DLI.                                                    
012209*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
012309     03  W-IDDC-B6-X.                                                     
012409         05  W-IDDC-B6           PIC X(2).                                
012509                                                                          
012609     03  W-IDPRC-MIN-X.                                                   
012709         05  W-IDPRC-MIN         PIC X(4)    VALUE SPACE.                 
012809                                                                          
012909     03  W-IDPRC-X.                                                       
013009         05  W-IDPRC             PIC X(4)    VALUE SPACE.                 
013109                                                                          
013209     03  W-KDKOLLI-K5-X.                                                  
013309         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
013409                                                                          
013509     03  W-WDGXKEY-4447-X.                                                
013609         05  W-IDHDTYP-4447      PIC X(4)    VALUE '4447'.                
013709         05  W-IDDC-4447         PIC X(2).                                
013809         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
013909                                                                          
014009     03  W-WDGXKEY-4448-X.                                                
014109         05  W-IDPRC-4448        PIC X(4)    VALUE SPACE.                 
014209         05  FILLER              PIC X       VALUE LOW-VALUE.             
014309                                                                          
014409*    --- STATUS-KOD FRÅN IMS                                              
014509 01  STATUS-WS                   PIC XX.                                  
014609     88  SEGMENT-FINNS                       VALUE '  '.                  
014709     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014809     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014909     SKIP2                                                                
015009 01  GODK-STATUSKODER.                                                    
015109     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015209     SKIP3                                                                
015309 01  SSA1                        PIC X(64).                               
015409 01  SSA2                        PIC X(64).                               
015509     EJECT                                                                
015609*    --- IMS FUNKTIONSKODER                                               
015709*01  -COPY W0003                                                          
015809     EJECT                                                                
015909*    ---  DLI INPUT-OUTPUT AREA                                           
016009                                                                          
016109 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4448'.                    
016209 01  DLI-IO-WDGX4448.                                                     
016309*    03  -COPY WDGX4448                                                   
016409 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
016509 01  DLI-IO-WDK501.                                                       
016609*    03  -COPY WDK501                                                     
016709 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
016809 01  DLI-IO-WDB601.                                                       
016909*    03  -COPY WDB601                                                     
017009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB612'.                      
017109 01  DLI-IO-WDB612.                                                       
017209*    03  -COPY WDB612                                                     
017309     EJECT                                                                
017409 LINKAGE SECTION.                                                         
017509*01  -COPY W0009   -PRE MSG-                                              
017609*01  -COPY W0008   -PRE WDP7-                                             
017709     05  FILLER                  PIC X.                                   
017809                                                                          
017909*01  -COPY W0008  -PRE 4448-                                              
018009     05  FILLER                  PIC X.                                   
018109                                                                          
018209*01  -COPY W0008  -PRE WDK5-                                              
018309     05  FILLER                  PIC X.                                   
018409                                                                          
018509*01  -COPY W0008  -PRE WDB6-                                              
018609     05  FILLER                  PIC X.                                   
018709     EJECT                                                                
018809 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 4448-PCB WDK5-PCB             
018909     WDB6-PCB.                                                            
019009 MAIN SECTION.                                                            
019109     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 4448-PCB WDK5-PCB             
019209     WDB6-PCB.                                                            
019309                                                                          
019409     PERFORM IMS-GET-MSG                                                  
019509     IF SEGMENT-FINNS                                                     
019609       PERFORM A-INIT                                                     
019709       PERFORM B-KOLLA-NYCKLAR                                            
019809       IF NYCKLAR-OK                                                      
019909         IF MFS-UPDATE                                                    
020009            PERFORM G-KOLLA-INPUT                                         
020109            IF INDATA-OK                                                  
020209               PERFORM H-UPPDATERA                                        
020309            END-IF                                                        
020409         ELSE                                                             
020509           IF MFS-FIRST                                                   
020609             PERFORM C-FOERSTA-SIDA                                       
020709           ELSE                                                           
020809             IF MFS-NEXT                                                  
020909               PERFORM D-NAESTA-SIDA                                      
021009             ELSE                                                         
021109               PERFORM E-SAMMA-SIDA                                       
021209             END-IF                                                       
021309           END-IF                                                         
021409         END-IF                                                           
021509         PERFORM F-LAES-VISA-INFO                                         
021609       END-IF                                                             
021709*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
021809*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
021909       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40601 + 4                      
022009       PERFORM IMS-INSERT-MSG                                             
022109     END-IF                                                               
022209                                                                          
022309     MOVE ZERO TO RETURN-CODE                                             
022409     GOBACK                                                               
022509     .                                                                    
022609     EJECT                                                                
022709 A-INIT SECTION.                                                          
022809     MOVE 'A-INIT'         TO CURRENT-SECTION                             
022909                                                                          
023009     IF MSG-DUBBLA-TRANSKODER                                             
023109       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40601                 
023209       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023309       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023409     ELSE                                                                 
023509       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40601                  
023609       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023709       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023809     END-IF                                                               
023909                                                                          
024009     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024110                                                                          
024120     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024140                                                                          
024209     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024309                                                                          
024409     MOVE LOW-VALUE   TO MSG-AREA                                         
024509     MOVE 'W4O406N1'  TO MFS-IDMOD                                        
024609     MOVE '4406'      TO MOD-IDTRANS                                      
024709     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024809                                                                          
024909     IF EGEN-MID OR HELP-MID                                              
024910       CONTINUE                                                           
025109     ELSE                                                                 
025209       MOVE SPACE TO MFS-KDTRTYP                                          
025309       MOVE '7'    TO MFS-IDPFK                                           
025409     END-IF                                                               
025509     .                                                                    
025609     EJECT                                                                
025709 B-KOLLA-NYCKLAR SECTION.                                                 
025809     MOVE 'B-KOLLA'         TO CURRENT-SECTION                            
025909                                                                          
026009     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026109     MOVE '001'             TO MSGI-KDCALL                                
026209     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026309     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026409     MOVE '4406'            TO MSGI-IDTRANS                               
026509     IF GODK-MID OR EGEN-MID                                              
026609        MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                              
026709     ELSE                                                                 
026809        MOVE '++'           TO MSGI-IDDC-KEY                              
026909     END-IF                                                               
027009     IF GODK-MID                                                          
027109         MOVE MID-IDPRC-IN     TO MSGI-IDPRC                              
027309     END-IF                                                               
027409     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
027509     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
027609                                                                          
027709*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
027809     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
027810     IF MID-IDDC-IN = ALL '+' OR SPACE                                    
027820      MOVE SPAR-IDDC-ENTER  TO MSGI-IDDC-KEY                              
027821                               MID-IDDC-IN                                
027830     END-IF                                                               
027909                                                                          
028009     MOVE JA TO NYCKLAR-SW                                                
028109                                                                          
028209                                                                          
028309*    -- KONTROLL AV IDDC                                                  
028409     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
028509                                                                          
028609     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
028610                           W-IDDC-4447                                    
028709     PERFORM IMS-GU-WDB601                                                
028809                                                                          
028909     IF SEGMENT-SAKNAS                                                    
029009       MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                                
029109       CALL WMEDKONV USING MED-WMEDAREA                                   
029209       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029309       PERFORM MFS-RENSA-FAELT-UT                                         
029409       MOVE NEJ TO NYCKLAR-SW                                             
029509     END-IF                                                               
029609                                                                          
029709     MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                  
029809                                                                          
029909     IF NYCKLAR-FEL                                                       
030009       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030109       CALL WMEDKONV USING MED-WMEDAREA                                   
030209       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
030309       PERFORM MFS-RENSA-FAELT-IN                                         
030409       PERFORM MFS-RENSA-FAELT-UT                                         
030509     END-IF                                                               
030609     .                                                                    
030709     EJECT                                                                
030809 C-FOERSTA-SIDA SECTION.                                                  
030909     MOVE 'C-FOERSTA'         TO CURRENT-SECTION                          
031009                                                                          
031109     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
031209     CALL WMEDKONV USING MED-WMEDAREA                                     
031309     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
031409                                                                          
031509     PERFORM MFS-RENSA-FAELT-IN                                           
031609     .                                                                    
031709     EJECT                                                                
031809 D-NAESTA-SIDA SECTION.                                                   
031909     MOVE 'D-NAESTA'         TO CURRENT-SECTION                           
032009                                                                          
032109     IF SPAR-IDTRANS = '4406'                                             
032209       MOVE SPAR-IDDC-NEXT  TO W-IDDC-B6                                  
032309       MOVE SPAR-IDPRC-NEXT TO W-IDPRC                                    
032409     ELSE                                                                 
032509       PERFORM MFS-RENSA-FAELT-IN                                         
032609     END-IF                                                               
032709     .                                                                    
032809     EJECT                                                                
032909 E-SAMMA-SIDA SECTION.                                                    
033009     MOVE 'E-SAMMA '         TO CURRENT-SECTION                           
033109                                                                          
033209     IF SPAR-IDTRANS = '4406' OR '0551'                                   
033309       MOVE SPAR-IDDC-ENTER  TO W-IDDC-B6                                 
033409       MOVE SPAR-IDPRC-ENTER TO W-IDPRC                                   
033509       IF MID-INPUT = ALL '+'                                             
033609         PERFORM MFS-RENSA-FAELT-IN                                       
033709       ELSE                                                               
033809         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
033909         CALL WMEDKONV USING MED-WMEDAREA                                 
034009         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
034109         PERFORM EA-MID-INDATA-TILL-MOD                                   
034209       END-IF                                                             
034309     ELSE                                                                 
034409       PERFORM MFS-RENSA-FAELT-IN                                         
034509     END-IF                                                               
034609     .                                                                    
034709     EJECT                                                                
034809 EA-MID-INDATA-TILL-MOD SECTION.                                          
034909                                                                          
035009* * * * FÖR VARJE MID-FÄLT                                                
035109* * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDATA-        
035209* * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR          
035309* * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                     
035409     IF MID-KDCMD-IN NOT = ALL '+'                                        
035509       MOVE MID-KDCMD-IN       TO MOD-KDCMD-IN                            
035609     END-IF                                                               
035709     IF MID-IDPRC-IN NOT = ALL '+'                                        
035809       MOVE MID-IDPRC-IN       TO MOD-IDPRC-IN                            
035909     END-IF                                                               
036009     IF MID-IDKOLLI-PRCSTA-IN NOT = ALL '+'                               
036109       MOVE MID-IDKOLLI-PRCSTA-IN   TO MOD-IDKOLLI-PRCSTA-IN              
036209     END-IF                                                               
036309     IF MID-KDKOLLI-IN NOT = ALL '+'                                      
036409       MOVE MID-KDKOLLI-IN     TO MOD-KDKOLLI-IN                          
036509     END-IF                                                               
036510     PERFORM MFS-LAES-IN-IGEN                                             
036609     .                                                                    
036709     EJECT                                                                
036809 F-LAES-VISA-INFO SECTION.                                                
036909     MOVE 'F-LAES-VISA'      TO CURRENT-SECTION                           
037009                                                                          
037109     PERFORM FA-LAES-GRUNDDATA                                            
037209                                                                          
037309     IF SEGMENT-SAKNAS                                                    
037409        MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                               
037509        CALL WMEDKONV USING MED-WMEDAREA                                  
037609        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
037709        PERFORM MFS-RENSA-FAELT-UT                                        
037809     ELSE                                                                 
037909*      -- POSITIONERA FÖR LÄSNING AV DATA TILL ÖVERSTA RADEN              
038009*      -- (EJ NÖDVÄNDIGT OM -MIN NYCKLAR ANVÄNDS DIREKT I SSA)            
038109*      MOVE W-IDPRC-MIN       TO W-IDPRC                                  
038209                                                                          
038309       MOVE +1 TO INDX                                                    
038409       PERFORM FB-LAES-RADDATA                                            
038509       IF SEGMENT-FINNS                                                   
038609         MOVE W-IDDC-B6       TO SPAR-IDDC-ENTER                          
038709         MOVE W-IDDC-B6       TO SPAR-IDDC-NEXT                           
038809         MOVE PRC-IDPRC       TO SPAR-IDPRC-ENTER                         
038909         MOVE PRC-IDPRC       TO SPAR-IDPRC-NEXT                          
039009       ELSE                                                               
039109         MOVE W-IDDC-B6       TO SPAR-IDDC-ENTER                          
039210         MOVE W-IDPRC-MIN     TO SPAR-IDPRC-ENTER                         
039309       END-IF                                                             
039409                                                                          
039509       PERFORM UNTIL INDX > MAX-INDX                                      
039609         IF SEGMENT-FINNS                                                 
039709           MOVE PRC-IDPRC       TO MOD-IDPRC (INDX)                       
039809           MOVE PRC-IDKOLLI-PRCSTA TO MOD-IDKOLLI-PRCSTA (INDX)           
039909           MOVE PRC-KDKOLLI     TO MOD-KDKOLLI (INDX)                     
040009           PERFORM FB-LAES-RADDATA                                        
040109         ELSE                                                             
040209           MOVE MFS-RENSA-FAELT TO MOD-IDPRC (INDX)                       
040309                                   MOD-IDKOLLI-PRCSTA(INDX)               
040409                                   MOD-KDKOLLI (INDX)                     
040509         END-IF                                                           
040609         ADD 1 TO INDX                                                    
040709       END-PERFORM                                                        
040809                                                                          
040909       IF SEGMENT-FINNS                                                   
041009         MOVE W-IDDC-B6         TO SPAR-IDDC-NEXT                         
041109         MOVE PRC-IDPRC         TO SPAR-IDPRC-NEXT                        
041209         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
041309         CALL WMEDKONV USING MED-WMEDAREA                                 
041409         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
041509       ELSE                                                               
041609         MOVE W-IDDC-B6         TO SPAR-IDDC-NEXT                         
041709         MOVE PRC-IDPRC         TO SPAR-IDPRC-NEXT                        
041809       END-IF                                                             
041909                                                                          
042009       MOVE '002'      TO MSGI-KDCALL                                     
042109       MOVE '4406'   TO SPAR-IDTRANS                                      
042209       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
042309       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
042409     END-IF                                                               
042410     IF NOT MFS-UPDATE                                                    
042420       PERFORM MFS-RENSA-FAELT-IN                                         
042430     END-IF                                                               
042509     .                                                                    
042609     EJECT                                                                
042709 FA-LAES-GRUNDDATA SECTION.                                               
042809     MOVE 'FA-LAES-GRUNDDATA'  TO CURRENT-SECTION                         
042909                                                                          
042910     IF MID-IDDC-IN NOT > SPACE                                           
042920       MOVE MSGI-IDDC-KEY    TO MID-IDDC-IN                               
042930     END-IF                                                               
043009     MOVE MID-IDDC-IN     TO W-IDDC-B6                                    
043109                             SPAR-IDDC-ENTER                              
043209                             SPAR-IDDC-NEXT                               
043309     PERFORM IMS-GU-WDB601                                                
043409                                                                          
043509     .                                                                    
043609     EJECT                                                                
043709 FB-LAES-RADDATA SECTION.                                                 
043809     MOVE 'FB-LAES-RADDATA'    TO CURRENT-SECTION                         
043909                                                                          
044009     PERFORM IMS-GNP-WDB612                                               
044010                                                                          
044109     .                                                                    
044209     EJECT                                                                
044309 G-KOLLA-INPUT SECTION.                                                   
044409     MOVE 'G-KOLLA-INPUT'    TO CURRENT-SECTION                           
044509                                                                          
044609     MOVE JA  TO INDATA-SW                                                
044709     IF MID-INPUT = ALL '+'                                               
044809       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044909       CALL WMEDKONV USING MED-WMEDAREA                                   
045009       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045109       PERFORM MFS-ROER-EJ-FAELT-UT                                       
045209       MOVE NEJ TO INDATA-SW                                              
045309     ELSE                                                                 
045409       IF INDATA-OK                                                       
045509          PERFORM GD-KOLLA-KDCMD                                          
045609       END-IF                                                             
045709                                                                          
045809       IF INDATA-OK                                                       
045909          PERFORM GA-KOLLA-IDPRC                                          
046009       END-IF                                                             
046109                                                                          
046209       IF INDATA-OK                                                       
046309          AND INSRT OR ANDRA OR NEW OR EDIT                               
046409          PERFORM GB-KOLLA-IDKOLLI                                        
046509       END-IF                                                             
046609                                                                          
046709       IF INDATA-OK                                                       
046710          AND INSRT OR ANDRA OR NEW OR EDIT                               
046909          PERFORM GC-KOLLA-KDKOLLI                                        
047009       END-IF                                                             
047109     END-IF                                                               
047209     .                                                                    
047309     EJECT                                                                
047409 GA-KOLLA-IDPRC  SECTION.                                                 
047509     MOVE 'GA-KOLLA-IDPRC'     TO CURRENT-SECTION                         
047609                                                                          
047709     MOVE MSGI-IDPRC           TO W-IDPRC-4448                            
047809     PERFORM IMS-GU-WDGX4448                                              
047909     IF SEGMENT-SAKNAS                                                    
048009       MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                       
048109       CALL WMEDKONV USING MED-WMEDAREA                                   
048209       MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                   
048210       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-IN-ATTR                       
048309*      PERFORM MFS-RENSA-FAELT-UT                                         
048409       MOVE NEJ         TO INDATA-SW                                      
048509     ELSE                                                                 
048609       MOVE MID-IDPRC-IN          TO W-IDPRC                              
048709       PERFORM IMS-GU-WDB612                                              
048710       IF SEGMENT-SAKNAS AND (ANDRA OR EDIT OR DLETE)                     
048720         OR SEGMENT-FINNS AND (INSRT OR NEW)                              
048910         OR (SEGMENT-FINNS AND DLETE AND                                  
048911*          DEFAULT 9999 ÅSTE FINNAS                                       
048920         MSGI-IDPRC = '9999')                                             
049009         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
049109         CALL WMEDKONV USING MED-WMEDAREA                                 
049209         MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                 
049210         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-IN-ATTR                     
049309*        PERFORM MFS-RENSA-FAELT-UT                                       
049409         MOVE NEJ         TO INDATA-SW                                    
049509       END-IF                                                             
049510     END-IF                                                               
049609     .                                                                    
049709     EJECT                                                                
049809 GB-KOLLA-IDKOLLI SECTION.                                                
049909     MOVE 'GB-KOLLA-IDKOLLI'   TO CURRENT-SECTION                         
050009                                                                          
050109     IF MID-IDKOLLI-PRCSTA-IN NOT NUMERIC                                 
050209       MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                       
050309       CALL WMEDKONV USING MED-WMEDAREA                                   
050409       MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                   
050509*      PERFORM MFS-RENSA-FAELT-UT                                         
050510       MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-PRCSTA-IN-ATTR               
050609       MOVE NEJ         TO INDATA-SW                                      
050709     END-IF                                                               
050809     .                                                                    
050909     EJECT                                                                
051009 GC-KOLLA-KDKOLLI SECTION.                                                
051109     MOVE 'GC-KOLLA-KDKOLLI'   TO CURRENT-SECTION                         
051209                                                                          
051309     MOVE MID-KDKOLLI-IN       TO W-KDKOLLI-K5                            
051409     PERFORM IMS-GU-WDK501                                                
051509     IF SEGMENT-SAKNAS                                                    
051609       MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                       
051709       CALL WMEDKONV USING MED-WMEDAREA                                   
051809       MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                   
051910       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDKOLLI-IN-ATTR                   
052009       MOVE NEJ         TO INDATA-SW                                      
052109     END-IF                                                               
052209     .                                                                    
052309     EJECT                                                                
052409 GD-KOLLA-KDCMD   SECTION.                                                
052509     MOVE 'GD-KOLLA-KDCMD '   TO CURRENT-SECTION                          
052609                                                                          
052709     IF MID-KDCMD-IN                 NOT = ALL '+'                        
052809       IF MID-KDCMD-IN  = 'I' OR 'C' OR 'D' OR 'N' OR 'E'                 
052910         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDCMD-IN-ATTR                  
052920         MOVE MID-KDCMD-IN          TO UPPD-SW                            
053009       ELSE                                                               
053109         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDCMD-IN-ATTR                  
053209         MOVE NEJ                   TO INDATA-SW                          
053309       END-IF                                                             
053409     ELSE                                                                 
053509        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDCMD-IN-ATTR                  
053609     END-IF                                                               
053610     PERFORM MFS-ROER-EJ-FAELT-IN                                         
053709     .                                                                    
053809     EJECT                                                                
053909 H-UPPDATERA   SECTION.                                                   
054009     MOVE 'H-UPPDATERA  '    TO CURRENT-SECTION                           
054109                                                                          
054209     IF INSRT OR NEW                                                      
054309       PERFORM HA-INSERT-PRC                                              
054409     END-IF                                                               
054509     IF ANDRA OR EDIT                                                     
054609       PERFORM HB-REPLACE-PRC                                             
054709     END-IF                                                               
054809     IF DLETE                                                             
054909       PERFORM HC-DELETE-PRC                                              
055009     END-IF                                                               
055010     PERFORM MFS-RENSA-FAELT-IN                                           
055109     .                                                                    
055209     EJECT                                                                
055309 HA-INSERT-PRC   SECTION.                                                 
055409     MOVE 'HA-INSERT-PRC'     TO CURRENT-SECTION                          
055509                                                                          
055609     MOVE MID-IDPRC-IN        TO PRC-IDPRC                                
055709     MOVE MID-IDKOLLI-PRCSTA-IN TO PRC-IDKOLLI-PRCSTA                     
055809     MOVE MID-KDKOLLI-IN      TO PRC-KDKOLLI                              
055909     PERFORM IMS-ISRT-WDB612                                              
056009     .                                                                    
056109     EJECT                                                                
056209 HB-REPLACE-PRC   SECTION.                                                
056309     MOVE 'HB-REPLACE-PRC'    TO CURRENT-SECTION                          
056409                                                                          
056509     MOVE MID-IDPRC-IN          TO W-IDPRC                                
056609     PERFORM IMS-GHU-WDB612                                               
056709     IF SEGMENT-FINNS                                                     
056809       MOVE MID-IDPRC-IN        TO PRC-IDPRC                              
056909       MOVE MID-IDKOLLI-PRCSTA-IN  TO PRC-IDKOLLI-PRCSTA                  
057009       MOVE MID-KDKOLLI-IN      TO PRC-KDKOLLI                            
057109       PERFORM IMS-REPL-WDB612                                            
057209     END-IF                                                               
057309     .                                                                    
057409     EJECT                                                                
057509 HC-DELETE-PRC   SECTION.                                                 
057609     MOVE 'HC-DELETE-PRC'     TO CURRENT-SECTION                          
057709                                                                          
057809     MOVE MID-IDPRC-IN          TO W-IDPRC                                
057909     PERFORM IMS-GHU-WDB612                                               
058009     IF SEGMENT-FINNS                                                     
058109       MOVE MID-IDPRC-IN        TO PRC-IDPRC                              
058209       PERFORM IMS-DLET-WDB612                                            
058309     END-IF                                                               
058409     .                                                                    
058509     EJECT                                                                
058609 MFS-RENSA-FAELT-UT SECTION.                                              
058709                                                                          
058809*    --- ALLA UTDATA-FÄLT                                                 
058909*    --- INKL. BLÄDDRINGSNYCKLAR                                          
059009     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
059109     .                                                                    
059209     SKIP3                                                                
059309 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
059409                                                                          
059509*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
059609     MOVE MFS-RENSA-FAELT   TO MOD-IDPRC (INDX)                           
059709                               MOD-IDKOLLI-PRCSTA (INDX)                  
059809                               MOD-KDKOLLI (INDX)                         
059909     .                                                                    
060009     SKIP3                                                                
060109 MFS-RENSA-FAELT-IN SECTION.                                              
060209                                                                          
060309*    --- ALLA INDATA-FÄLT                                                 
060409     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
060509                               MOD-KDCMD-IN                               
060609                               MOD-IDPRC-IN                               
060709                               MOD-IDKOLLI-PRCSTA-IN                      
060809                               MOD-KDKOLLI-IN                             
060909     .                                                                    
061009     EJECT                                                                
061109 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
061209                                                                          
061309*    --- ALLA UTDATA-FÄLT                                                 
061409*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
061509     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-UT                                
061609     MOVE +1 TO INDX                                                      
061709     PERFORM UNTIL INDX > MAX-INDX                                        
061809       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
061909       ADD +1 TO INDX                                                     
062009     END-PERFORM                                                          
062109     .                                                                    
062209     SKIP2                                                                
062309 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
062409                                                                          
062509*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
062609     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRC (INDX)                          
062709                                MOD-IDKOLLI-PRCSTA (INDX)                 
062809                                MOD-KDKOLLI (INDX)                        
062909     .                                                                    
063009     SKIP3                                                                
063109 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
063209                                                                          
063309*    --- ALLA INDATA-FÄLT                                                 
063409     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDC-IN                               
063509                                MOD-KDCMD-IN                              
063609                                MOD-IDPRC-IN                              
063709                                MOD-IDKOLLI-PRCSTA-IN                     
063809                                MOD-KDKOLLI-IN                            
063909     .                                                                    
064009     EJECT                                                                
064109 MFS-FORM-ATTR SECTION.                                                   
064209                                                                          
064309*    --- ALLA INDATA-FÄLT                                                 
064409     MOVE MFS-FORMATETS-ATTR   TO MOD-IDKOLLI-PRCSTA-IN-ATTR              
064509                                  MOD-KDKOLLI-IN-ATTR                     
064609                                  MOD-IDPRC-IN-ATTR                       
064709                                  MOD-KDCMD-IN-ATTR                       
064809     .                                                                    
064909     SKIP2                                                                
065009 MFS-LAES-IN-IGEN SECTION.                                                
065109                                                                          
065209*    --- ALLA INDATA-FÄLT                                                 
065309     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRC-IN-ATTR                      
065409                                   MOD-KDCMD-IN-ATTR                      
065509                                   MOD-IDKOLLI-PRCSTA-IN-ATTR             
065609                                   MOD-KDKOLLI-IN-ATTR                    
065709     .                                                                    
065809     EJECT                                                                
065909* --- IMS SEKTIONER ---                                                   
066009     SKIP3                                                                
066109 IMS-GET-MSG SECTION.                                                     
066209                                                                          
066309     MOVE '  QC' TO GODK-STATUSKODER                                      
066409     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066509     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
066609     PERFORM IMS-STATUSKONTROLL                                           
066709     .                                                                    
066809     SKIP3                                                                
066909 IMS-INSERT-MSG SECTION.                                                  
067009                                                                          
067409     MOVE LOW-VALUE     TO MSG-KDZ1 MSG-KDZ2                              
067509     MOVE SPACE         TO GODK-STATUSKODER                               
067609     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067709     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
067809     PERFORM IMS-STATUSKONTROLL                                           
067909     .                                                                    
068009     EJECT                                                                
068109 IMS-GU-WDGX4448 SECTION.                                                 
068209     MOVE 'IMS-GU-WDGX4448'    TO CURRENT-SECTION                         
068309                                                                          
068409     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
068509          DELIMITED BY SIZE INTO SSA1                                     
068609     STRING 'WDR160  (WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
068709          DELIMITED BY SIZE INTO SSA2                                     
068809     MOVE '  GE'             TO GODK-STATUSKODER                          
068909     CALL CBLTDLI USING GU 4448-PCB DLI-IO-WDGX4448 SSA1 SSA2             
069009     MOVE 4448-STATUS-CODE   TO STATUS-WS                                 
069109     PERFORM IMS-STATUSKONTROLL                                           
069209     .                                                                    
069309     EJECT                                                                
069409 IMS-GU-WDK501 SECTION.                                                   
069509     MOVE 'IMS-GU-WDK501'    TO CURRENT-SECTION                           
069609                                                                          
069709     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-K5-X ')'                      
069809          DELIMITED BY SIZE INTO SSA1                                     
069909     MOVE '  GE'             TO GODK-STATUSKODER                          
070009     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
070109     MOVE WDK5-STATUS-CODE   TO STATUS-WS                                 
070209     PERFORM IMS-STATUSKONTROLL                                           
070309     .                                                                    
070409     EJECT                                                                
070509 IMS-GU-WDB601 SECTION.                                                   
070609     MOVE 'IMS-GU-WDB601'    TO CURRENT-SECTION                           
070709                                                                          
070809     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
070909          DELIMITED BY SIZE INTO SSA1                                     
071009     MOVE '  GE'             TO GODK-STATUSKODER                          
071109     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
071209     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
071309     PERFORM IMS-STATUSKONTROLL                                           
071409     .                                                                    
071509     EJECT                                                                
071609 IMS-GHU-WDB612 SECTION.                                                  
071709     MOVE 'IMS-GHU-WDB612'   TO CURRENT-SECTION                           
071809                                                                          
071909     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
072009          DELIMITED BY SIZE INTO SSA1                                     
072109     STRING 'WDB612  (IDPRC    =' W-IDPRC-X ')'                           
072209          DELIMITED BY SIZE INTO SSA2                                     
072309     MOVE '  GE'             TO GODK-STATUSKODER                          
072409     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB612 SSA1 SSA2              
072509     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
072609     PERFORM IMS-STATUSKONTROLL                                           
072709     .                                                                    
072809     EJECT                                                                
072810 IMS-GU-WDB612 SECTION.                                                   
072820     MOVE 'IMS-GU-WDB612'    TO CURRENT-SECTION                           
072830                                                                          
072840     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
072850          DELIMITED BY SIZE INTO SSA1                                     
072860     STRING 'WDB612  (IDPRC    =' W-IDPRC-X ')'                           
072870          DELIMITED BY SIZE INTO SSA2                                     
072880     MOVE '  GE'             TO GODK-STATUSKODER                          
072890     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB612 SSA1 SSA2               
072900     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
072901     PERFORM IMS-STATUSKONTROLL                                           
072902     .                                                                    
072903     EJECT                                                                
072909 IMS-GNP-WDB612 SECTION.                                                  
073009     MOVE 'IMS-GNP-WDB612'    TO CURRENT-SECTION                          
073109                                                                          
073209     STRING 'WDB612  (IDPRC   =>' W-IDPRC-X ')'                           
073309          DELIMITED BY SIZE INTO SSA1                                     
073409     MOVE '  GE'             TO GODK-STATUSKODER                          
073509     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB612 SSA1                   
073609     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
073709     PERFORM IMS-STATUSKONTROLL                                           
073809     .                                                                    
073909     EJECT                                                                
074009 IMS-ISRT-WDB612 SECTION.                                                 
074109     MOVE 'IMS-ISRT' TO CURRENT-SECTION                                   
074209                                                                          
074309     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
074409          DELIMITED BY SIZE INTO SSA1                                     
074509     MOVE   'WDB612  '        TO SSA2                                     
074609     MOVE '  ' TO GODK-STATUSKODER                                        
074709     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB612 SSA1 SSA2             
074809     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
074909     PERFORM IMS-STATUSKONTROLL                                           
075009     .                                                                    
075010 IMS-REPL-WDB612 SECTION.                                                 
075020     MOVE 'IMS-REPL' TO CURRENT-SECTION                                   
075030                                                                          
075070     MOVE '  ' TO GODK-STATUSKODER                                        
075080     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB612                       
075090     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
075100     PERFORM IMS-STATUSKONTROLL                                           
075101     .                                                                    
075109 IMS-DLET-WDB612 SECTION.                                                 
075209     MOVE 'IMS-DLET' TO CURRENT-SECTION                                   
075309                                                                          
075409     MOVE '  ' TO GODK-STATUSKODER                                        
075509     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB612                       
075609     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
075709     PERFORM IMS-STATUSKONTROLL                                           
075809     .                                                                    
075909 IMS-STATUSKONTROLL SECTION.                                              
076009                                                                          
076109     SET STATUS-IX TO 1                                                   
076209     SEARCH GODK-STATUS                                                   
076309       AT END                                                             
076409         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
076509         DELIMITED BY SIZE INTO FELTEXT                                   
076609         CALL FELLOG                                                      
076709       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
076809         CONTINUE                                                         
076909     END-SEARCH                                                           
077009     .                                                                    
