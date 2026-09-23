000418 ID DIVISION.                                                             
000419 PROGRAM-ID.    W0050300.                                                 
000420 AUTHOR.        RICHARD.                                                  
000500 DATE-WRITTEN.  APRIL 75.                                                 
000510 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   WINFO (INFORMATIONS-PROGRAM).                            
000710*                ÄVEN:                                                    
000720*                OM ETT PROGRAM EJ ÄR KLART                               
000800*                ELLER SKALL UTGÅ                                         
000810*                LÄNKAS DETTA PROGRAM IN MED DET ANDRA                    
000820*                PROGRAMMETS NAMN OCH GER DÅ DEN UPPLYSNING               
000830*                SOM BEHÖVS.                                              
000900     SKIP3                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100     SKIP3                                                                
001200 DATA DIVISION.                                                           
001300     EJECT                                                                
001400 WORKING-STORAGE SECTION.                                                 
001401                                                                          
001410*    -- CHECKED BY WY2000                                                 
001500 77    IDPGM                 PIC X(8)    VALUE 'W0050300'.                
001501 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
001502 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
001510 77    JA                    PIC X       VALUE 'J'.                       
001520 77    NEJ                   PIC X       VALUE 'N'.                       
001530 77    W-KDTRANS             PIC X(6)    VALUE SPACE.                     
001540     SKIP3                                                                
001550 01    W-VIMSID.                                                          
001560   03    W-IMSID             PIC X(4)    VALUE SPACE.                     
001570   03    FILLER              PIC X(4)    VALUE SPACE.                     
001590     SKIP3                                                                
001600 01    DYNAMISKA-SUBPROGRAM.                                              
001700   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
001800   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
001810   03    VIMSID              PIC X(8)    VALUE 'VIMSID  '.                
001910     EJECT                                                                
002000******************************************************************        
002100*                                                                         
002200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
002300*                                                                         
002400 01    FILLER                PIC X(16)   VALUE '     MFS-WS     '.        
002500                                                                          
002600 01    MID.                                                               
002702   03    FILLER              PIC X(100).                                  
002703     EJECT                                                                
002706*01    -COPY WMSGAREA                                                     
002730   03    MOD    REDEFINES MSG-AREA.                                       
002731     05    MOD-IDTRANS       PIC X(4).                                    
002732     05    MOD-KDMFSFOR      PIC X(1).                                    
002733     05    MOD-RAD-05        PIC X(79).                                   
002734     05    MOD-RAD-06        PIC X(79).                                   
002735     05    MOD-RAD-07        PIC X(79).                                   
002736     05    MOD-RAD-08        PIC X(79).                                   
002737     05    MOD-RAD-09        PIC X(79).                                   
002738     05    MOD-RAD-10        PIC X(79).                                   
002739     05    MOD-RAD-11        PIC X(79).                                   
002740     05    MOD-RAD-12        PIC X(79).                                   
002741     05    MOD-RAD-13        PIC X(79).                                   
002742     05    MOD-RAD-14        PIC X(79).                                   
002743     05    MOD-RAD-15        PIC X(79).                                   
002744     05    MOD-RAD-16        PIC X(79).                                   
002745     05    MOD-RAD-17        PIC X(79).                                   
002746     05    MOD-RAD-18        PIC X(79).                                   
002747     05    MOD-RAD-19        PIC X(79).                                   
002748     05    MOD-RAD-20        PIC X(79).                                   
002750     05    MOD-TEMFSINF      PIC X(55).                                   
002790     EJECT                                                                
002800*01    -COPY WMSGSPAR                                                     
003000     EJECT                                                                
003100******************************************************************        
003200*                                                                         
003300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
003400*                                                                         
003500 01    IMS-WS.                                                            
003600   03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.             
003700     SKIP3                                                                
003800   03    STATUS-WS           PIC XX.                                      
003900     88  SEGMENT-FINNS                   VALUE '  '.                      
004000     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
004020     88  STATUS-OK                       VALUE '  '.                      
004050     88  TRANSKOD-FEL                    VALUE 'A1'.                      
004060     88  SECURITY-FEL                    VALUE 'A4'.                      
004070     88  FLERA-SVAR-FINNS                VALUE 'CC'.                      
004100     SKIP3                                                                
004200   03    GODK-STATUSKODER.                                                
004300     05    GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.              
004400     EJECT                                                                
004500*01    -COPY W0003                                                        
004700     EJECT                                                                
004800 LINKAGE SECTION.                                                         
004900*01    -COPY W0009     -PRE MSG-                                          
005100     EJECT                                                                
005200 PROCEDURE DIVISION USING MSG-PCB.                                        
005400 MAIN SECTION.                                                            
005410     ENTRY 'DLITCBL' USING MSG-PCB.                                       
005500                                                                          
005510     PERFORM IMS-GET-MSG                                                  
005600     IF SEGMENT-FINNS                                                     
005700       PERFORM A-INIT-SPARA-INPUT                                         
005800       IF W-KDTRANS = 'WINFO ' OR 'W0T503'                                
005895         PERFORM C-SEND-INFO                                              
005900       ELSE                                                               
005910         PERFORM B-SEND-ANNAN                                             
006000       END-IF                                                             
006100       CALL VIMSID USING W-IMSID                                          
006200       IF W-IMSID = 'IMG0'                                                
006210         STRING W-IMSID '   PULS PRODUKTIONS IMS'                         
006220           DELIMITED BY SIZE INTO MOD-TEMFSINF                            
006230       ELSE                                                               
006240         IF W-IMSID = 'IMD0' OR 'IMY0' OR 'IMP0' OR 'IMB0'                
006250           STRING W-IMSID '   PULS TEST IMS  '                            
006260             DELIMITED BY SIZE INTO MOD-TEMFSINF                          
006270         ELSE                                                             
006280             STRING W-IMSID '   VCAS UTBILDNINGS IMS'                     
006290               DELIMITED BY SIZE INTO MOD-TEMFSINF                        
006291         END-IF                                                           
006292       END-IF                                                             
006298       PERFORM IMS-INSERT-MSG                                             
006299     END-IF                                                               
006300     MOVE ZERO TO RETURN-CODE                                             
006310     GOBACK                                                               
006400     .                                                                    
007600     EJECT                                                                
007700 A-INIT-SPARA-INPUT SECTION.                                              
007800                                                                          
007801     MOVE WHEN-COMPILED TO W-COMPILED                                     
007802     MOVE MSG-KDTRANS-1 TO W-KDTRANS                                      
007803                                                                          
007810     IF MSG-DUBBLA-TRANSKODER                                             
007900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID                          
008000       MOVE MSG-IDTRANS-2 TO MSG-SPAR-IDTRANS                             
008100       MOVE MSG-KDMFSFOR-2 TO MSG-SPAR-KDMFSFOR                           
008200     ELSE                                                                 
008300       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID                            
008400       MOVE MSG-IDTRANS-1 TO MSG-SPAR-IDTRANS                             
008500       MOVE MSG-KDMFSFOR-1 TO MSG-SPAR-KDMFSFOR                           
008600     END-IF                                                               
008700     MOVE LOW-VALUE TO MSG-AREA                                           
008710     MOVE 'W0O50301' TO MSG-SPAR-MODNAMN                                  
008720     MOVE '0503' TO MOD-IDTRANS                                           
008721     MOVE MSG-SPAR-KDMFSFOR TO MOD-KDMFSFOR                               
008730     MOVE SPACE TO MOD-TEMFSINF                                           
008740     .                                                                    
008741     EJECT                                                                
008742 B-SEND-ANNAN SECTION.                                                    
008743                                                                          
008744       EVALUATE MSG-SPAR-IDTRANS                                          
008757         WHEN '1106'                                                      
008758           MOVE '6211' TO MOD-IDTRANS                                     
008759           MOVE SPACE TO MOD-KDMFSFOR                                     
008760           IF MSG-SPAR-KDMFSFOR = '2'                                     
008761             MOVE 'W6O211N1' TO MSG-SPAR-MODNAMN                          
008762             MOVE 'PICTURE 11062 HAS BEEN REPLACED BY 62112'              
008763               TO MOD-RAD-05                                              
008764           ELSE                                                           
008765             MOVE 'W6O21101' TO MSG-SPAR-MODNAMN                          
008766             MOVE 'BILD 1106 HAR ERSATTS AV BILD 62111'                   
008767               TO MOD-RAD-05                                              
008768           END-IF                                                         
008769       END-EVALUATE                                                       
008770       MOVE +48 TO MSG-KVLL                                               
008775     .                                                                    
008776     EJECT                                                                
008777 C-SEND-INFO SECTION.                                                     
008778                                                                          
008779     IF MSG-SPAR-KDMFSFOR = '2'                                           
008780       MOVE '031018           VCN->Ford network migration                 
008781-        '                          '                                     
008782         TO MOD-RAD-05                                                    
008783       MOVE 'Due to the Net Migration external systems can not be         
008784-        'reached until they        '                                     
008785         TO MOD-RAD-06                                                    
008786       MOVE 'migrate week 4 2004. Until then log on in a separate         
008787-        'terminal window with      '                                     
008788         TO MOD-RAD-07                                                    
008789       MOVE 'command: IMSPV and use the specific system as start-c        
008790-        'ommand (acfi, KDP, TIKO,  '                                     
008791         TO MOD-RAD-08                                                    
008792       MOVE 'SI, DMENYVH etc etc). Sesam users should contact thei        
008793-        'r sesam admins to get new '                                     
008794         TO MOD-RAD-09                                                    
008795       MOVE 'sesam entry lines for the external systems.                  
008796-        '                          '                                     
008797         TO MOD-RAD-10                                                    
008799       MOVE '                                                             
008800-        '                          '                                     
008801         TO MOD-RAD-11                                                    
008806       MOVE ' Problems? or Questions?   Please call;                      
008807-        '                          '                                     
008808         TO MOD-RAD-12                                                    
008809       MOVE 'PULS Helpdesk +46 +31 3270120                                
008810-        '                          '                                     
008811         TO MOD-RAD-13                                                    
008812       MOVE 'support.vccspulshelpdesk@volvo.com                           
008813-        '                          '                                     
008814         TO MOD-RAD-14                                                    
008815       MOVE '                                                             
008816-        '                          '                                     
008817         TO MOD-RAD-15                                                    
008818       MOVE '!!!  To reach CBR (99980) you will need to logon IMSV        
008819-        '1 and use DMENYVH as start'                                     
008820         TO MOD-RAD-16                                                    
008821       MOVE 'command. !!!                                                 
008822-        '                          '                                     
008823         TO MOD-RAD-17                                                    
008824       MOVE '                                                             
008825-        '                          '                                     
008826         TO MOD-RAD-18                                                    
008827       MOVE '                                                             
008828-        '                          '                                     
008829         TO MOD-RAD-19                                                    
008830       MOVE '                                                             
008831-        '                          '                                     
008832         TO MOD-RAD-20                                                    
008833     ELSE                                                                 
008837       MOVE '031018  VCN->Fords nätverks migrering                        
008838-        '                          '                                     
008839         TO MOD-RAD-05                                                    
008840       MOVE '                                           (TYPE IN          
008841-        '(05032 FOR ENGLISH INFO)  '                                     
008842         TO MOD-RAD-06                                                    
008843       MOVE '                                                             
008844-        '                          '                                     
008845         TO MOD-RAD-07                                                    
008846       MOVE 'Pga nät migreringen så kan inte längre externa system        
008847-        ' nås direkt från PULS     '                                     
008848         TO MOD-RAD-08                                                    
008849       MOVE 'innan de också migrerar vecka 4 2004. Tills dess logg        
008850-        'a på i en separat terminal'                                     
008851         TO MOD-RAD-09                                                    
008852       MOVE 'med IMSPV och använd det externa systemet som start-c        
008853-        'ommand (acpi, KDP, Tiko,  '                                     
008854         TO MOD-RAD-10                                                    
008855       MOVE 'SI, etc). Sesam användare kan kontakta sin sesam admi        
008856-        'nistratör   '                                                   
008857         TO MOD-RAD-11                                                    
008858       MOVE 'att få en specifik rad till det externa systemet.            
008859-        '                          '                                     
008860         TO MOD-RAD-12                                                    
008864       MOVE '                                                             
008865-        '                          '                                     
008866         TO MOD-RAD-13                                                    
008867       MOVE 'Har du problem eller frågor ring;                            
008868-        '                          '                                     
008869         TO MOD-RAD-14                                                    
008871       MOVE 'PULS Helpdesk +46 +31 3270120                                
008872-        '                          '                                     
008873         TO MOD-RAD-15                                                    
008874       MOVE 'support.vccspulshelpdesk@volvo.com                           
008875-        '                          '                                     
008876         TO MOD-RAD-16                                                    
008877       MOVE '                                                             
008878-        '                          '                                     
008879         TO MOD-RAD-17                                                    
008880       MOVE '                                                             
008881-        '                          '                                     
008882         TO MOD-RAD-18                                                    
008883       MOVE 'obs. För att nå CBR (99980) så måste man logga på IMS        
008884-        'V1 och ange DMENYVH som   '                                     
008885         TO MOD-RAD-19                                                    
008886       MOVE 'start command. obs                                           
008887-        '                          '                                     
008888         TO MOD-RAD-20                                                    
008889     END-IF                                                               
008890     MOVE +1328 TO MSG-KVLL                                               
008891     .                                                                    
008900     EJECT                                                                
008909* IMS SEKTIONER                                                           
008910                                                                          
008911 IMS-GET-MSG SECTION.                                                     
008912     MOVE '  QC' TO GODK-STATUSKODER                                      
008913     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
008920     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
009000     PERFORM IMS-STATUSKONTROLL                                           
009100     .                                                                    
009200     SKIP3                                                                
009300 IMS-INSERT-MSG SECTION.                                                  
009400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
009500     MOVE SPACE TO GODK-STATUSKODER                                       
009600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MSG-SPAR-MODNAMN         
009700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
009800     PERFORM IMS-STATUSKONTROLL                                           
009900     .                                                                    
010000     SKIP3                                                                
010100 IMS-STATUSKONTROLL SECTION.                                              
010200     SET STATUS-IX TO 1                                                   
010300     SEARCH GODK-STATUS                                                   
010400       AT END                                                             
010500         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
010510         CALL FELLOG                                                      
010600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
010700         CONTINUE                                                         
010800     END-SEARCH                                                           
010900     .                                                                    
