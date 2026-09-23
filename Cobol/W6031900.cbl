000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0112      *        
000400******************************************************************        
000500*                                                                         
001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6031900.                                                
001500 AUTHOR.         TOMMIE JIVARP.                                           
001600 DATE-WRITTEN.   98/02/25.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        FRÅGEBILD FÖR ARTIKELS LAGERPLATSHISTORIK                        
002100*                                                                         
002201*        PROGRAMMET LÄSER      WLLOCB (WDJ9)                              
002210*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002220*        PROGRAMMET LÄSER              WDB6                               
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W6T319                                              
002600*        MID:         W6I31901                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W6O31901                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6031900'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004420                                                                          
004423                                                                          
004430 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004440                                                                          
004450*    --- MELLANLAGRING                                                    
004470 77  W-TISTADAT                  PIC 9(8)    VALUE ZERO.                  
004471 77  W-TISTODAT                  PIC 9(8)    VALUE ZERO.                  
004472 77  W-KDLOC                     PIC X       VALUE SPACE.                 
004480                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '6319'.                
005600     88  GODK-MID                            VALUE '6311' '6312'          
005700                                                   '6313' '6314'          
005800                                                   '6315' '6316'          
005900                                                   '6317' '6318'          
006000                                                   '6319'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006210                                                                          
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007010                                                                          
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007620     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007810     03  ERR-PART-MISSING-DC     PIC X(3)    VALUE '305'.                 
007820     03  ERR-CORR-HILITE-FIELDS  PIC X(3)    VALUE '409'.                 
007830     03  ERR-PARTHIST-MISSING    PIC X(3)    VALUE '214'.                 
007840     03  ERR-DC-MISSING          PIC X(3)    VALUE '423'.                 
007900     EJECT                                                                
007910                                                                          
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502                                                                          
008503*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008505 01  SPAR-AREA.                                                           
008506     03  SPAR-IDTRANS            PIC X(4)    VALUE '6319'.                
008507                                                                          
008512     03  SPAR-WDJ911KY-ENTER.                                             
008513         05  W-IDDC-ENTER        PIC X(2)    VALUE SPACE.                 
008514         05  W-DASTADAT-ENTER    PIC S9(9)   VALUE ZERO COMP-3.           
008515         05  W-TISTATID-ENTER    PIC S9(7)   VALUE ZERO COMP-3.           
008516         05  W-ADLAGOMR-ENTER    PIC 9(2)    VALUE ZERO.                  
008517         05  W-ADGANG-ENTER      PIC 9(2)    VALUE ZERO.                  
008518         05  W-ADPLATS-ENTER     PIC 9(5)    VALUE ZERO.                  
008519                                                                          
008520     03  SPAR-WDJ911KY-NEXT.                                              
008530         05  W-IDDC-NEXT         PIC X(2)    VALUE SPACE.                 
008540         05  W-DASTADAT-NEXT     PIC S9(9)   VALUE ZERO COMP-3.           
008550         05  W-TISTATID-NEXT     PIC S9(7)   VALUE ZERO COMP-3.           
008560         05  W-ADLAGOMR-NEXT     PIC 9(2)    VALUE ZERO.                  
008570         05  W-ADGANG-NEXT       PIC 9(2)    VALUE ZERO.                  
008580         05  W-ADPLATS-NEXT      PIC 9(5)    VALUE ZERO.                  
008600     EJECT                                                                
008610                                                                          
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I31901                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O31901                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010310                                                                          
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010810                                                                          
011001*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011002 01  W-WDJ911KY-MIN.                                                      
011003     03  W-IDDC-MIN-X.                                                    
011004         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
011005     03  W-DASTADAT-MIN-X.                                                
011006         05  W-DASTADAT-MIN      PIC S9(9)   COMP-3   VALUE ZERO.         
011007     03  W-TISTATID-MIN-X.                                                
011008         05  W-TISTATID-MIN      PIC S9(7)   COMP-3   VALUE ZERO.         
011009     03  W-ADLAGOMR-MIN-X.                                                
011010         05  W-ADLAGOMR-MIN      PIC 9(2)    VALUE ZERO.                  
011011     03  W-ADGANG-MIN-X.                                                  
011012         05  W-ADGANG-MIN        PIC 9(2)    VALUE ZERO.                  
011013     03  W-ADPLATS-MIN-X.                                                 
011014         05  W-ADPLATS-MIN       PIC 9(5)    VALUE ZERO.                  
011015                                                                          
011016 01  NYCKLAR-TILL-DLI.                                                    
011017     03  W-IDARTNR-X.                                                     
011018         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
011021     03  W-WDJ911KY-X.                                                    
011022         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011023         05  W-DASTADAT          PIC S9(9)   COMP-3   VALUE ZERO.         
011024         05  W-TISTATID          PIC S9(7)   COMP-3   VALUE ZERO.         
011025         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
011026         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
011027         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
011028     03  W-IDSKYLT-X.                                                     
011030         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
011040                                                                          
011050     03  W-IDDC-B6-X.                                                     
011060         05 W-IDDC-B6            PIC X(2).                                
011070                                                                          
011100     SKIP2                                                                
011110                                                                          
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012310                                                                          
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012710                                                                          
012800*    ---  DLI INPUT-OUTPUT AREA                                           
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013002 01  DLI-IO-WLARTC01.                                                     
013003*    03  -COPY WDK601 -PRE ARTC-                                          
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB01'.                    
013006 01  DLI-IO-WLLOCB01.                                                     
013007*    03  -COPY WDJ901                                                     
013008     EJECT                                                                
013009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB11'.                    
013010 01  DLI-IO-WLLOCB11.                                                     
013011*    03  -COPY WDJ911                                                     
013012 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
013013 01  DLI-IO-WLBENA11.                                                     
013020*    03  -COPY WDD311  -PRE BENA-                                         
013030                                                                          
013040 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013050 01   DLI-IO-AREA-B601.                                                   
013060*     03  -COPY WDB601                                                    
013070                                                                          
013300     EJECT                                                                
013310                                                                          
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008   -PRE ARTC-                                             
013803      05 FILLER                  PIC X.                                   
013804     EJECT                                                                
013805*01  -COPY W0008   -PRE BENA-                                             
013810     05  FILLER                  PIC X.                                   
013811                                                                          
013820*01  -COPY W0008   -PRE LOCB-                                             
013830     05  FILLER                  PIC X.                                   
013831                                                                          
013832*01  -COPY W0008   -PRE WDB6-                                             
013833     05  FILLER                  PIC X.                                   
013840                                                                          
013900     EJECT                                                                
014000                                                                          
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
014002                                            LOCB-PCB WDB6-PCB.            
014003 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
014020                                            LOCB-PCB WDB6-PCB.            
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31901 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I31901                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I31901                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W6O319N1' TO MFS-IDMOD                                         
018300     MOVE '6319' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018403                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019510                                                                          
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '6319'            TO MSGI-IDTRANS                               
020300     MOVE JA TO NYCKLAR-SW                                                
020701                                                                          
021107                                                                          
021108     IF EGEN-MID                                                          
021109       MOVE MID-IDARTNR-IN  TO  MSGI-IDARTNR                              
021114     ELSE                                                                 
021120       IF MID-IDARTNR-IN NUMERIC                                          
021130         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
021140       END-IF                                                             
021150     END-IF                                                               
021235                                                                          
021236     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021237     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
021238                                                                          
021243     IF MSGI-IDLAND-SPR = 'GB'                                            
021244        MOVE 'GB ' TO W-IDSKYLT                                           
021245                     MED-IDSKYLT                                          
021246     ELSE                                                                 
021247        MOVE 'S  '  TO W-IDSKYLT                                          
021248                     MED-IDSKYLT                                          
021249     END-IF                                                               
021250                                                                          
021251     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021252     IF MSGI-IDARTNR NUMERIC                                              
021253       IF MSGI-IDARTNR > ZERO                                             
021254         MOVE MSGI-IDARTNR TO W-IDARTNR                                   
021255       END-IF                                                             
021256     ELSE                                                                 
021257       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
021258       MOVE NEJ TO NYCKLAR-SW                                             
021259     END-IF                                                               
021260                                                                          
021261     IF NYCKLAR-OK                                                        
021262       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
021263       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021264     ELSE                                                                 
021265       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021266     END-IF                                                               
021267                                                                          
021268*    -- KONTROLL AV IDDC                                                  
021269     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
021270     IF EGEN-MID                                                          
021271       IF MID-IDDC-IN NOT = ALL '+'                                       
021272         MOVE MID-IDDC-IN    TO W-IDDC-B6                                 
021273         PERFORM IMS-GU-WDB601                                            
021274         IF DCS-KDDC = SPACE OR DCS-DDC                                   
021275           MOVE ERR-DC-MISSING TO MED-IDMFSFEL                            
021276           MOVE MID-IDDC-IN TO MOD-IDDC-UT                                
021277           MOVE NEJ         TO NYCKLAR-SW                                 
021278         ELSE                                                             
021279           MOVE MID-IDDC-IN TO W-IDDC                                     
021280                               MOD-IDDC-UT                                
021282         END-IF                                                           
021287       ELSE                                                               
021288         MOVE MID-IDDC-UT   TO W-IDDC                                     
021289                               MOD-IDDC-UT                                
021290       END-IF                                                             
021291     ELSE                                                                 
021292       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
021293       PERFORM IMS-GU-WDB601                                              
021294       IF DCS-KDDC = SPACE OR DCS-DDC                                     
021295         MOVE NEJ           TO NYCKLAR-SW                                 
021296       ELSE                                                               
021297         MOVE MSGI-IDDC     TO W-IDDC                                     
021298                               MOD-IDDC-UT                                
021300       END-IF                                                             
021301     END-IF                                                               
021302     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
021303                                                                          
021304*    -- KONTROLL AV KDLOC                                                 
021305     MOVE MFS-RENSA-FAELT   TO MOD-KDLOC-IN                               
021306                                                                          
021307     IF EGEN-MID                                                          
021308       IF MID-KDLOC-IN NOT = ALL '+'                                      
021309         IF MID-KDLOC-IN = 'P'                                            
021310         OR MID-KDLOC-IN = 'B'                                            
021311         OR MID-KDLOC-IN = 'R'                                            
021312         OR MID-KDLOC-IN = 'T'                                            
021313         OR MID-KDLOC-IN = 'C'                                            
021314         OR MID-KDLOC-IN = 'S'                                            
021314         OR MID-KDLOC-IN = 'H'                                            
021315         OR MID-KDLOC-IN = SPACE                                          
021316           IF MID-KDLOC-IN = SPACE                                        
021317               MOVE 'T'            TO W-KDLOC                             
021318                                      MOD-KDLOC-UT                        
021319           ELSE                                                           
021320             MOVE MID-KDLOC-IN     TO W-KDLOC                             
021321                                      MOD-KDLOC-UT                        
021329           END-IF                                                         
021330         ELSE                                                             
021331           MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                            
021332           MOVE MID-KDLOC-IN       TO MOD-KDLOC-UT                        
021333           MOVE NEJ                TO NYCKLAR-SW                          
021334         END-IF                                                           
021335       ELSE                                                               
021336         MOVE MID-KDLOC-UT         TO W-KDLOC                             
021337                                      MOD-KDLOC-UT                        
021338       END-IF                                                             
021339     ELSE                                                                 
021340       MOVE 'T'                    TO W-KDLOC                             
021341                                      MOD-KDLOC-UT                        
021342     END-IF                                                               
021343                                                                          
021350     IF NYCKLAR-FEL                                                       
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102                                                                          
022103 C-FOERSTA-SIDA SECTION.                                                  
022104                                                                          
022105     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022106     CALL WMEDKONV USING MED-WMEDAREA                                     
022107     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
022108                                                                          
022109     PERFORM MFS-RENSA-FAELT-IN                                           
022110                                                                          
022111*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
022112     MOVE SPACE              TO SPAR-WDJ911KY-ENTER                       
022113                                SPAR-WDJ911KY-NEXT                        
022114     .                                                                    
022115     EJECT                                                                
022116                                                                          
022117 D-NAESTA-SIDA SECTION.                                                   
022118                                                                          
022125     IF SPAR-WDJ911KY-NEXT NOT = SPACE                                    
022126       MOVE SPAR-WDJ911KY-NEXT                                            
022127                             TO W-WDJ911KY-X                              
022129     END-IF                                                               
022130     MOVE SPACE              TO SPAR-WDJ911KY-ENTER                       
022131                                SPAR-WDJ911KY-NEXT                        
022132     .                                                                    
022133     EJECT                                                                
022134                                                                          
022135 E-SAMMA-SIDA SECTION.                                                    
022136                                                                          
022150     IF SPAR-WDJ911KY-ENTER NOT = SPACE                                   
022151       MOVE SPAR-WDJ911KY-ENTER                                           
022152                             TO W-WDJ911KY-MIN                            
022154     END-IF                                                               
022155     MOVE SPACE              TO SPAR-WDJ911KY-ENTER                       
022156                                SPAR-WDJ911KY-NEXT                        
022157     .                                                                    
022158     EJECT                                                                
022159                                                                          
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600     PERFORM IMS-GU-LOCB01                                                
022800     IF SEGMENT-SAKNAS                                                    
022801       MOVE ERR-PART-MISSING         TO MED-IDMFSFEL                      
022802       CALL WMEDKONV USING MED-WMEDAREA                                   
022803       MOVE MED-TEMFSFEL             TO MOD-TEMFSFEL                      
023300     ELSE                                                                 
023310       MOVE +1 TO INDX                                                    
023404       PERFORM FA-LAES-LOCB11                                             
023405                                                                          
023430       PERFORM UNTIL INDX > MAX-INDX                                      
023432       OR SEGMENT-SAKNAS                                                  
023434       OR HIST-IDDC > W-IDDC                                              
023435         IF INDX = 1                                                      
023451           MOVE HIST-IDDC                TO W-IDDC                        
023452           MOVE HIST-DASTADAT-9KOMPL TO W-DASTADAT                        
023453           MOVE HIST-TISTATID-9KOMPL TO W-TISTATID                        
023454           MOVE HIST-ADLAGOMR            TO W-ADLAGOMR                    
023455           MOVE HIST-ADGANG              TO W-ADGANG                      
023456           MOVE HIST-ADPLATS             TO W-ADPLATS                     
023457           MOVE W-WDJ911KY-X             TO SPAR-WDJ911KY-ENTER           
023458         END-IF                                                           
023459                                                                          
023460         COMPUTE W-TISTADAT =                                             
023461                            99999999 - HIST-DASTADAT-9KOMPL               
023462         MOVE W-TISTADAT (3:6)           TO MOD-TISTADAT   (INDX)         
023463         MOVE HIST-ADLAGOMR              TO MOD-ADLAGOMR   (INDX)         
023464         MOVE HIST-ADGANG                TO MOD-ADGANG     (INDX)         
023465         MOVE HIST-ADPLATS               TO MOD-ADPLATS    (INDX)         
023468         MOVE HIST-KDLOC                 TO MOD-KDLOC      (INDX)         
023470         MOVE HIST-DASTODAT              TO W-TISTODAT                    
023471         MOVE W-TISTODAT (3:6)           TO MOD-TISTODAT   (INDX)         
023472         MOVE HIST-IDUSER                TO MOD-IDUSER     (INDX)         
023473         MOVE HIST-IDUSER-STO            TO MOD-IDUSER-STO (INDX)         
023474         PERFORM FA-LAES-LOCB11                                           
023476         ADD +1 TO INDX                                                   
023485       END-PERFORM                                                        
023486*      OM FLER RADER FINNS                                                
023487       IF SEGMENT-FINNS AND HIST-IDDC = W-IDDC                            
023488         MOVE HIST-IDDC                  TO W-IDDC                        
023489         MOVE HIST-DASTADAT-9KOMPL       TO W-DASTADAT                    
023490         MOVE HIST-TISTATID-9KOMPL       TO W-TISTATID                    
023491         MOVE HIST-ADLAGOMR              TO W-ADLAGOMR                    
023492         MOVE HIST-ADGANG                TO W-ADGANG                      
023493         MOVE HIST-ADPLATS               TO W-ADPLATS                     
023494         MOVE W-WDJ911KY-X               TO SPAR-WDJ911KY-NEXT            
023496         MOVE INF-MORE-INFO-EXISTS       TO MED-IDMFSINF                  
023497         CALL WMEDKONV USING MED-WMEDAREA                                 
023498         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023499       ELSE                                                               
023501         MOVE INF-LAST-PAGE              TO MED-IDMFSINF                  
023503         CALL WMEDKONV USING MED-WMEDAREA                                 
023504         MOVE MED-TEMFSINF               TO MOD-TEMFSINF                  
023505         MOVE SPACE                      TO MOD-TEMFSFEL                  
023508       END-IF                                                             
023509       IF INDX = 1                                                        
023510         MOVE ERR-PARTHIST-MISSING TO MED-IDMFSFEL                        
023511         CALL WMEDKONV USING MED-WMEDAREA                                 
023512         MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                      
023525       END-IF                                                             
023544                                                                          
023545       PERFORM IMS-GU-BENA01-BSEQ                                         
023546       IF SEGMENT-FINNS                                                   
023547         MOVE BENA-TEXT-BEART TO MOD-BEART                                
023548       ELSE                                                               
023549         MOVE SPACE           TO MOD-BEART                                
023550       END-IF                                                             
023551                                                                          
023552       PERFORM IMS-GU-K601                                                
023553       IF SEGMENT-FINNS                                                   
023554         MOVE ARTC-ART-REKSIFFR      TO MOD-REKSIFFR                      
023555         MOVE '-'                    TO MOD-STRECK                        
023556       ELSE                                                               
023557         MOVE ZERO                   TO MOD-REKSIFFR                      
023558         MOVE SPACE                  TO MOD-STRECK                        
023559       END-IF                                                             
023560                                                                          
023561       MOVE '002'      TO MSGI-KDCALL                                     
023562       MOVE '6319'     TO SPAR-IDTRANS                                    
023563       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023564       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023570     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023701                                                                          
023702 FA-LAES-LOCB11 SECTION.                                                  
023703                                                                          
023720     PERFORM IMS-GNP-LOCB11                                               
023721                                                                          
023730     PERFORM UNTIL SEGMENT-SAKNAS                                         
023740     OR    HIST-IDDC > W-IDDC                                             
023750     OR    HIST-KDLOC = W-KDLOC                                           
023760     OR    W-KDLOC = 'T'                                                  
023770     OR   (W-KDLOC = 'R'                                                  
023780     AND   HIST-KDLOC = 'B')                                              
023790     OR   (W-KDLOC = 'P'                                                  
023800     AND  (HIST-KDLOC = 'C'                                               
023900     OR    HIST-KDLOC = 'S'                                               
023900     OR    HIST-KDLOC = 'H'))                                             
024000       PERFORM IMS-GNP-LOCB11                                             
024100     END-PERFORM                                                          
025773     .                                                                    
025774     EJECT                                                                
025775                                                                          
025780 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
026100                             MOD-IDDC-IN                                  
026110                             MOD-KDLOC-IN                                 
026200     .                                                                    
026300     EJECT                                                                
027009                                                                          
027100 MFS-ROER-EJ-FAELT-UT SECTION.                                            
027200                                                                          
027300*    --- ALLA UTDATA-FÄLT                                                 
027400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
027610                               MOD-STRECK                                 
027620                               MOD-REKSIFFR                               
027630                               MOD-BEART                                  
027700     .                                                                    
027800     EJECT                                                                
027810                                                                          
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030310                                                                          
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502                                                                          
031503 IMS-GU-K601 SECTION.                                                     
031504                                                                          
031505     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031506             DELIMITED BY SIZE INTO SSA1                                  
031507     MOVE '  GE' TO GODK-STATUSKODER                                      
031508     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031509     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031510     PERFORM IMS-STATUSKONTROLL                                           
031511     SKIP3                                                                
031512     .                                                                    
031513                                                                          
031514 IMS-GU-LOCB01 SECTION.                                                   
031515                                                                          
031516     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
031517          DELIMITED BY SIZE INTO SSA1                                     
031518     MOVE '  GE' TO GODK-STATUSKODER                                      
031519     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
031520     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
031521     PERFORM IMS-STATUSKONTROLL                                           
031522     .                                                                    
031523     EJECT                                                                
031524                                                                          
031537 IMS-GNP-LOCB11 SECTION.                                                  
031538                                                                          
031539     STRING 'WLLOCB11(WDJ911KY=>' W-WDJ911KY-X ')'                        
031540          DELIMITED BY SIZE INTO SSA1                                     
031541     MOVE '  GE' TO GODK-STATUSKODER                                      
031542     CALL CBLTDLI USING GNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                 
031543     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
031544     PERFORM IMS-STATUSKONTROLL                                           
031545     .                                                                    
031546     EJECT                                                                
031547                                                                          
031548 IMS-GU-BENA01-BSEQ SECTION.                                              
031549                                                                          
031550     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
031551          DELIMITED BY SIZE INTO SSA1                                     
031552     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031553          DELIMITED BY SIZE INTO SSA2                                     
031554     MOVE '  GE'               TO GODK-STATUSKODER                        
031555     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
031556     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
031557     PERFORM IMS-STATUSKONTROLL                                           
031560     .                                                                    
031570     EJECT                                                                
031571 IMS-GU-WDB601    SECTION.                                                
031572     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031573          DELIMITED BY SIZE INTO SSA1                                     
031574     MOVE '  GE' TO GODK-STATUSKODER                                      
031575     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031576     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031577     PERFORM IMS-STATUSKONTROLL                                           
031578     IF SEGMENT-SAKNAS                                                    
031579         MOVE SPACE TO DCS-KDDC                                           
031580     END-IF                                                               
031581     .                                                                    
031582                                                                          
031590                                                                          
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
