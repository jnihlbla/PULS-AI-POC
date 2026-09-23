000100 01  4498-WDGX4498.                                                       
000200*                                 LASTBÄRARE                              
000300*                                 KOLLI                                   
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 ( IDDISTR  + IDKUNDNR +                 
000600*                                   KDFAKTYP            +                 
000700*                                   IDORDNR7 + IDPRODNR +                 
000800*                                   IDKOLLI)                              
000900     03 4498-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 4498-IDKUNDNR        PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 4498-KDFAKTYP        PIC X.                                       
001600*                                 FAKTURATYP                              
001700*                                 INVOICE TYPE                            
001800     03 4498-IDKUNDRF        PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100     03 4498-IDORDNR5-FILLER REDEFINES 4498-IDKUNDRF.                     
002200        05 4498-IDORDNR5     PIC 9(5).                                    
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500        05 FILLER            PIC X(5).                                    
002600     03 4498-IDORDNR7-FILLER REDEFINES 4498-IDKUNDRF.                     
002700        05 4498-IDORDNR7     PIC 9(7).                                    
002800*                                 ORDERNUMMER                             
002900*                                 ORDER NUMBER                            
003000        05 FILLER            PIC X(3).                                    
003100     03 4498-IDPRODNR        PIC S9(7)           COMP-3.                  
003200*                                 PRODUKTIONSNUMMER                       
003300*                                 PRODUCTION NUMBER                       
003400     03 4498-IDKOLLI         PIC S9(5)           COMP-3.                  
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 4498-IDDEALER        PIC S9(7)           COMP-3.                  
003800*                                 DEALER KUNDNUMMER                       
003900     03 4498-IDKOLLI-SAMP    PIC S9(5)           COMP-3.                  
004000*                                 SAMPACKNINGSKOLLINUMMER                 
004100*                                 MIXED PACKING CASE NUMBER               
004200     03 4498-IDPSN           OCCURS 2 TIMES                               
004300                             PIC 9(3).                                    
004400*                                 PROPER SHIPPING NAME                    
004500*                                 PROPER SHIPPING NAME                    
004600     03 4498-KDKOLLI         PIC X(8).                                    
004700*                                 KOLLIKOD                                
004800*                                 KOLLI CODE                              
004900     03 4498-TIRFS           PIC S9(11)          COMP-3.                  
005000*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005100*                                 READY FOR SHIPMENT  YYMMDDHHMM          
005200     03 4498-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
005300*                                 ORDERVIKT BRUTTO (KG)                   
005400*                                 GROSS WEIGHT (KG)                       
005500     03 4498-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
005600*                                 ORDERVOLYM BRUTTO (M3)                  
005700*                                 GROSS VOLUME PER ORDER (M3)             
005800     03 4498-FLCROSS         PIC X.                                       
005900*                                 CROSS-DOCKING FLAGGA                    
006000*                                 CROSS DOCKING FLAG                      
006100     03 4498-FILLER          PIC X(14).                                   
006200*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
