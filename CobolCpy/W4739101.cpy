000010 01  001-W4739101.                                                        
000020*                                 001 - HUVUD TILL PACKAD-ORDR            
000030*                                       LISTA KVANT SVERIGE               
000040     03 001-IDPTYP           PIC X(3).                                    
000050*                                 POSTTYP                                 
000060     03 001-IDPRODNR         PIC S9(7)           COMP-3.                  
000070*                                 PRODUKTIONSNUMMER                       
000080     03 001-IDDISTR          PIC S9(5)           COMP-3.                  
000090*                                 DISTRIKTNUMMER                          
000100     03 001-IDKUNDNR         PIC S9(7)           COMP-3.                  
000110*                                 KUNDNUMMER                              
000120     03 001-IDKUNDRF         PIC X(10).                                   
000130*                                 KUNDENS REFERENS (ORDERID)              
000140     03 001-IDDC             PIC X(2).                                    
000150*                                 IDENTIFIERARE LAGER                     
000160     03 001-KDFRAKT          PIC S9(3)           COMP-3.                  
000170*                                 FRAKTSÄTT C1-C2 TILL KUND               
000180     03 001-KDORDKL          PIC S9              COMP-3.                  
000190      88 001-KDORDKL-VOR     VALUE +0.                                    
000200      88 001-KDORDKL-DAG     VALUE +1.                                    
000210      88 001-KDORDKL-2       VALUE +2.                                    
000220      88 001-KDORDKL-SNABB   VALUE +2.                                    
000230      88 001-KDORDKL-SPECIAL VALUE +3.                                    
000240      88 001-KDORDKL-KVANT   VALUE +4.                                    
000250      88 001-KDORDKL-SATS    VALUE +5.                                    
000260*                                 ORDERKLASS                              
000270     03 001-VLORDBTO         PIC S9(4)V9(3)      COMP-3.                  
000280*                                 ORDERVOLYM BRUTTO (M3)                  
000290     03 001-KVKOLLI          PIC S9(5)           COMP-3.                  
000300*                                 ANTAL KOLLI                             
000310     03 001-TIPACKN          PIC S9(7)           COMP-3.                  
000320*                                 PACKNINGSDATUM         (ÅÅMMDD)         
      *** END COPY W4739101    LENGTH=40                                        
