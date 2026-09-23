000100 01  LPF-WDD601.                                                          
000200*                                 LEVERANSPLANEFÖRSLAGSKÖ                 
000300*                                 FYSISK NYCKEL WDD601KY:                 
000400*                                 (IDDC + IDLEVNR + IDARTNR +             
000500*                                  IDANSK)                                
000600     03 LPF-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 LPF-IDLEVNR          PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 LPF-IDARTNR          PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 LPF-IDANSK           PIC S9(3)           COMP-3.                  
001600*                                 ANSKAFFARNUMMER                         
001700*                                 PROCURER NO.                            
001800     03 LPF-KDLPORS          OCCURS 3 TIMES                               
001900                             PIC S9(3)           COMP-3.                  
002000*                                 LEVERANSPLANEORSAK                      
002100     03 LPF-KDLEVPLF         PIC X.                                       
002200*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
002300*                                 CODE FOR APPROVAL OF SCHEDULE P         
002400*                                 ROPOSAL                                 
002500     03 LPF-BEART            PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700*                                 PART DESCRIPTION                        
002800     03 LPF-TIOMSPEC         PIC S9(5)           COMP-3.                  
002900*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
003000     03 LPF-TIUPPDAT         PIC S9(7)           COMP-3.                  
003100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003200*                                 UPDATING DATE     (YYMMDD)              
003300     03 LPF-TELPORSX         PIC X(10).                                   
003400*                                 LEVERANSPLANEORSAK VARNING              
003500*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
