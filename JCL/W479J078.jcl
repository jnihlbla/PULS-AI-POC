//W479J078 JOB (670W4790100W479J078,W100),'RTN W479Q1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W479    EXEC W479P078,CPU=4                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J078                                         
