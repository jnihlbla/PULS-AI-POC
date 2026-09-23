000100 01  UPLP-W22415.                                                         
000200*                                 UPP-FIL FÖR OMSPEC. LEV.PLAN            
000300*                                 POSTTYP = 001 BORTTAG OMSPEC            
000400*                                 IDARTNR, IDDC                           
000500*                                                                         
000600*                                 POSTTYP = 002 BORTTAG FORSLAG           
000700*                                 IDARTNR, IDDC, KDAVROP                  
000800*                                                                         
000900*                                 POSTTYP = 003 UPDATE WDK722             
001000*                                 IDARTNR, IDDC, KDLEVPLF,                
001100*                                 KDLPSP, TILPSP, TIOMSPEC                
001200*                                                                         
001300*                                 POSTTYP = 004 NYUPPL OMSPEC             
001400*                                 IDARTNR, IDDC, IDLEVNR,                 
001500*                                 DASPECST, KDLPORS-TAB(),                
001600*                                 KVBEST-PL, KDPLKOEP                     
001700*                                                                         
001800*                                 POSTTYP = 005 UPPDAT OMSPEC             
001900*                                 IDARTNR, IDDC, IDLEVNR,                 
002000*                                 KDLPORS-TAB(), KVBEST-PL,               
002100*                                 KDPLKOEP                                
002200*                                                                         
002300*                                 POSTTYP = 006 NYUPPL LEV                
002400*                                 IDARTNR, IDDC, IDLEVNR                  
002500*                                                                         
002600*                                 POSTTYP = 007 UPD AVROP                 
002700*                                 IDARTNR, IDDC, IDLEVNR,                 
002800*                                 KDAVROP, DAAVROP-AVS, KVAVROP           
002900*                                                                         
003000*                                 POSTTYP = 008 NYUPPL AVROP              
003100*                                 IDARTNR, IDDC, IDLEVNR,                 
003200*                                 KDAVROP, DAAVROP-AVS, TILEVDAG,         
003300*                                 TIAVRDAT-INL, TIAVRDAT-DISP.            
003400*                                 KVAVROP                                 
003500*                                                                         
003600     03 UPLP-IDPTYP          PIC X(3).                                    
003700*                                 POSTTYP                                 
003800*                                 RECORD TYPE                             
003900     03 UPLP-IDARTNR         PIC S9(9)           COMP-3.                  
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200     03 UPLP-IDDC            PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400*                                 WAREHOUSE IDENTIFIER                    
004500     03 UPLP-IDLEVNR         PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 UPLP-DAAVROP-AVS     PIC 9(6).                                    
004900*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005000*                                 (ÅÅÅÅVV)                                
005100     03 UPLP-DASPECST        PIC 9(6).                                    
005200*                                 SPECAD FR.O.M DATUM   (ÅÅÅÅVV)          
005300     03 UPLP-KDAVROP         PIC S9              COMP-3.                  
005400*                                 AVROPSKOD                               
005500*                                 CALLED                                  
005600     03 UPLP-KDLEVPLF        PIC X.                                       
005700*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
005800*                                 CODE FOR APPROVAL OF SCHEDULE P         
005900*                                 ROPOSAL                                 
006000     03 UPLP-KDLPORS-TAB     OCCURS 3 TIMES                               
006100                             PIC S9(3)           COMP-3.                  
006200*                                 LEVERANSPLANEORSAK                      
006300     03 UPLP-KDLPSP          PIC S9              COMP-3.                  
006400*                                 LEVERANSPLANESPÄRR                      
006500     03 UPLP-KDPLKOEP        PIC S9              COMP-3.                  
006600*                                 STATUS AVTALSKÖP (PLAN)                 
006700*                                 1=FÖRESLAGEN  2=GODKÄND                 
006800     03 UPLP-KVAVROP         PIC S9(7)           COMP-3.                  
006900*                                 AVROPSKVANTITET                         
007000     03 UPLP-KVBEST-PL       PIC S9(7)           COMP-3.                  
007100*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
007200     03 UPLP-TIAVRDAT-DISP   PIC S9(7)           COMP-3.                  
007300*                                 PLANERAT DISPONIBLEDATUM                
007400     03 UPLP-TIAVRDAT-INL    PIC S9(7)           COMP-3.                  
007500*                                 PLANERAT INLEVERANSDATUM                
007600     03 UPLP-TILEVDAG        PIC S9              COMP-3.                  
007700*                                 AVSÄNDNINGSDAG INOM VECKA               
007800*                                 DELIVERY WEEK DAY                       
007900     03 UPLP-TILPSP          PIC S9(5)           COMP-3.                  
008000*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
008100     03 UPLP-TIOMSPEC        PIC S9(5)           COMP-3.                  
008200*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
008300*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
