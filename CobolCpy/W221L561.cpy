000100 01  W221L561.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22156 MOT LEVERANSPLANEREG             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ROT            VALUE +601.                                  
000700      88 LAES-NASTA-UNDER-ROT                                             
000800                             VALUE +602.                                  
000900      88 GHU-WDD902          VALUE +603.                                  
001000      88 GHU-WDD905          VALUE +605.                                  
001100      88 GHU-WDD906          VALUE +606.                                  
001200      88 GHU-WDD924          VALUE +607.                                  
001300      88 REPL-WDD924         VALUE +608.                                  
001400      88 DELETE              VALUE +609.                                  
001500      88 LAES-INL-PARTI      VALUE +611.                                  
001600      88 GHU-WDD925          VALUE +612.                                  
001700*                                 ANROPSTYP       KDCALL-W221-002         
001800     03 FLAGGA-ANROP         PIC X.                                       
001900      88 POST-FINNS          VALUE 'J'.                                   
002000      88 POST-SAKNAS         VALUE 'N'.                                   
002100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002200     03 IO-AREA.                                                          
002300        05 IDARTNR           PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500        05 IDDC              PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 IDLEVNR           PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900        05 KVBR              PIC S9(7)           COMP-3.                  
003000*                                 BESTÄLLNINGSREST                        
003100        05 TIXLEVSP          PIC S9(5)           COMP-3.                  
003200*                                 SPÄRWDATUM EXTRA-LEVERANS  ÅÅVV         
003300        05 TIEXLEV           PIC S9(5)           COMP-3.                  
003400*                                 AVSÄNDNINGSVECKA EXTRALEVERANS          
003500*                                 (ÅÅVV)                                  
003600        05 KVEXLEV           PIC S9(7)           COMP-3.                  
003700*                                 EXTRALEVERANSKVANTITET                  
003800        05 KVBEST-PL         PIC S9(7)           COMP-3.                  
003900*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
004000        05 KDAVROP           PIC S9              COMP-3.                  
004100*                                 AVROPSKOD                               
004200        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
004300*                                 INLEVERANSDATUM (PLANERAD)              
004400*                                 (ÅÅVV)                                  
004500        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
004600*                                 AVSÄNDNINGSVECKA (PLANERAD)             
004700*                                 (ÅÅVV)                                  
004800        05 KVAVROP           PIC S9(7)           COMP-3.                  
004900*                                 AVROPSKVANTITET                         
005000        05 IDLOPNRM-PL       PIC S9(9)           COMP-3.                  
005100*                                 AVBOKNINGSID, (ÅÅVVDLLLL)               
005200        05 KVAVROP-AVB       PIC S9(7)           COMP-3.                  
005300*                                 AVBOKAT ANTAL                           
005400        05 TILEVBSK-AVS      PIC S9(7)           COMP-3.                  
005500*                                 LEV. BESK. AVS. DAT.(ÅÅMMDD)            
005600        05 TILEVBSK-INLC1    PIC S9(7)           COMP-3.                  
005700*                                 LEV. BESK. INLEV. DAT(ÅÅMMDD)           
005800        05 TILEVBSK-INLC2    PIC S9(7)           COMP-3.                  
005900*                                 LEV. BESK. INLEV. DAT(ÅÅMMDD)           
006000        05 KVAVIS-BSKKVARC1  PIC S9(7)           COMP-3.                  
006100*                                 LEV. BESK. ANT. EFTER AVBOKNING         
006200        05 KVAVIS-BSKKVARC2  PIC S9(7)           COMP-3.                  
006300*                                 LEV. BESK. ANT. EFTER AVBOKNING         
006400        05 FLSENLEVC1        PIC X.                                       
006500*                                 FÖRSENAD LEVERANS                       
006600        05 FLSENLEVC2        PIC X.                                       
006700*                                 FÖRSENAD LEVERANS                       
006800        05 FLKLAR            PIC X.                                       
006900*                                 AVSLUTNINGSMARKERING                    
007000        05 IDLEVBSK          PIC S9              COMP-3.                  
007100*                                 TYP AV LEVERANSBESKEDSTEXT              
007200        05 TELEVBSK          PIC X(80).                                   
007300*                                 LEVERANSBESKEDSINFORMATION              
007400        05 TIBORT            PIC S9(7)           COMP-3.                  
007500*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
007600*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
