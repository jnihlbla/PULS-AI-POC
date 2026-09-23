000100 01  AVSP-W403AVSP.                                                       
000200*                                 LÄNKAREA TILL W403AVSP -                
000300*                                 PACKNING AV ORDDERDEL OCH               
000400*                                 AVSLUT AV PACKNING OM SISTA             
000500*                                 ORDERDELEN PACKAS.                      
000600*                                                                         
000700*                                 UTAREA:                                 
000800*                                 KDSVAR FÅS MED                          
000900*                                 BLANK = OK                              
001000*                                     F = FEL                             
001100     03 AVSP-INDATA.                                                      
001200*                                 INDATA TILL W403AVSP                    
001300        05 AVSP-IDTRANS      PIC X(4).                                    
001400*                                 BILDNUMMER                              
001500        05 AVSP-IDANSTNR     PIC 9(5).                                    
001600*                                 ANSTÄLLNINGSNUMMER                      
001700        05 AVSP-IDDC         PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900        05 AVSP-IDPRODNR     PIC 9(7).                                    
002000*                                 PRODUKTIONSNUMMER                       
002100        05 AVSP-IDDISTR      PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300        05 AVSP-IDKUNDNR     PIC 9(6).                                    
002400*                                 KUNDNUMMER                              
002500        05 AVSP-IDORDNR      PIC 9(5).                                    
002600*                                 ORDERNUMMER UTGÅR PD90                  
002700        05 AVSP-IDORDER      PIC 9(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900        05 AVSP-IDPLKLST     PIC 9(3).                                    
003000*                                 PLOCKLISTNUMMER                         
003100        05 AVSP-IDKOLLI      PIC 9(5).                                    
003200*                                 KOLLINUMMER                             
003300        05 AVSP-KDKOLLI      PIC X(8).                                    
003400*                                 KOLLIKOD                                
003500        05 AVSP-VKORDBTO     PIC 9(6)V9(1).                               
003600*                                 ORDERVIKT BRUTTO (KG)                   
003700        05 AVSP-KDEMBTYP     PIC 9.                                       
003800*                                 EMBALLAGETYP                            
003900        05 AVSP-DIKOLLIL     PIC 9(4).                                    
004000*                                 KOLLI-LÄNGD                             
004100        05 AVSP-DIKOLLIB     PIC 9(3).                                    
004200*                                 KOLLI-BREDD                             
004300        05 AVSP-DIKOLLIH     PIC 9(3).                                    
004400*                                 KOLLI-HÖJD                              
004500        05 AVSP-KDKOLLID     PIC 9.                                       
004600*                                 KOLLI-DJUP                              
004700        05 AVSP-VKORDNTO     PIC 9(6)V9(1).                               
004800*                                 ORDERVIKT NETTO (KG)                    
004900        05 AVSP-IDKOLLI-SAMP PIC 9(5).                                    
005000*                                 SAMPACKNINGSKOLLINUMMER                 
005100        05 AVSP-VKTARA       PIC 9(6)V9(1).                               
005200*                                 TARAVIKT (KG)                           
005300     03 AVSP-UTDATA.                                                      
005400*                                 UTDATA TILL W403AVSP                    
005500        05 AVSP-KDSVAR       PIC X.                                       
005600         88 AVSP-OK          VALUE ' '.                                   
005700         88 AVSP-SAKNAS      VALUE 'S'.                                   
005800         88 AVSP-FEL         VALUE 'F'.                                   
005900*                                                       KDSVAR-88         
006000*                                 SVARSKOD FRÅN SUBPROGRAM                
006100        05 AVSP-ERROR-MESSAGE                                             
006200                             PIC 9(3).                                    
006300*** END OF VILMAII-COPY LENGTH= 98 BYTES                                  
