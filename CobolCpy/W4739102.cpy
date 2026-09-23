000100 01  002-W4739102.                                                        
000200*                                 002 - KOLLI TILL PACKAD-ORDER           
000300*                                       LISTA KVANT SVERIGE               
000400     03 002-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 002-IDPRODNR         PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800     03 002-IDKOLLI          PIC S9(5)           COMP-3.                  
000900*                                 KOLLINUMMER                             
001000     03 002-KDKOLLI          PIC X(8).                                    
001100*                                 KOLLIKOD                                
001200     03 002-KDEMBTYP         PIC S9              COMP-3.                  
001300*                                 EMBALLAGETYP                            
001400     03 002-KVPALL           PIC S9(7)           COMP-3.                  
001500*                                 ANTAL PALLAR                            
001600     03 002-KVRAM            PIC S9(3)           COMP-3.                  
001700*                                 ANTAL RAMAR                             
001800     03 002-KVLOCK           PIC S9(5)           COMP-3.                  
001900*                                 ANTAL  LOCK                             
002000     03 002-DIKOLLIL         PIC S9(5)           COMP-3.                  
002100*                                 KOLLI-LÄNGD                             
002200     03 002-DIKOLLIB         PIC S9(3)           COMP-3.                  
002300*                                 KOLLI-BREDD                             
002400     03 002-DIKOLLIH         PIC S9(3)           COMP-3.                  
002500*                                 KOLLI-HÖJD                              
002600     03 002-VLORDBTO-KOLLI   PIC S9(4)V9(3)      COMP-3.                  
002700*                                 ORDERVOLYM BRUTTO KOLLI                 
002800     03 002-TIPACKN          PIC S9(7)           COMP-3.                  
002900*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003000*** END COPY W4739102C0  LENGTH=43                                        
