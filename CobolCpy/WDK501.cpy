000100 01  EMB-WKOLLI.                                                          
000200*                                 ROTSEGMENT I KOLLIKODSREGISTRET         
000300*                                 FYSISK NYCKEL KDKOLLI                   
000400     03 EMB-KDKOLLI          PIC X(8).                                    
000500*                                 KOLLIKOD                                
000600     03 EMB-KDEMBTYP         PIC S9              COMP-3.                  
000700*                                 EMBALLAGETYP                            
000800     03 EMB-DIKOLLIL         PIC S9(5)           COMP-3.                  
000900*                                 KOLLI-LÄNGD                             
001000     03 EMB-DIKOLLIB         PIC S9(3)           COMP-3.                  
001100*                                 KOLLI-BREDD                             
001200     03 EMB-DIKOLLIH         PIC S9(3)           COMP-3.                  
001300*                                 KOLLI-HÖJD                              
001400     03 EMB-KDKOLLID         PIC X.                                       
001500*                                 KOLLI-DJUP                              
001600     03 EMB-VKTARA           PIC S9(6)V9(1)      COMP-3.                  
001700*                                 TARAVIKT (KG)                           
001800     03 EMB-KVPALL           PIC S9(3)           COMP-3.                  
001900*                                 ANTAL PALLAR         KVPALL-003         
002000     03 EMB-KVINTPALL        PIC S9(3)           COMP-3.                  
002100*                                 ANTAL INTERNPALLAR   KVINTPALL          
002200     03 EMB-KVRAM            PIC S9(3)           COMP-3.                  
002300*                                 ANTAL RAMAR                             
002400     03 EMB-KVLOCK           PIC S9(3)           COMP-3.                  
002500*                                 ANTAL LOCK                              
002600     03 EMB-EMBPROF          PIC X.                                       
002700*                                 EMBALLAGE PROFORMA-MÄRKNING             
002800     03 EMB-KVEMBSPA         PIC S9              COMP-3.                  
002900*                                 ANTAL SPACE-EMBALLAGE                   
003000     03 EMB-FILLER           PIC X(9).                                    
003100*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
