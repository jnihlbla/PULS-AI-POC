//W116J08A JOB (640W1160100W116J08A,W100),'RTN W116SF',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*   VCOM=&VCOM                                                                
//*   COUNTRYX2= &COUNTRYX2                                                     
//*                                                                             
//W116    EXEC W116P08A                                                         
//SORT01.SYSIN DD *                                                             
 SORT FIELDS=(1,2,CH,A),EQUALS                                                  
 INCLUDE COND=(262,2,CH,EQ,C'&COUNTRYX2')                                       
 SUM  FIELDS=(NONE)                                                             
 END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J08A                                         
