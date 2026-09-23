//W116J0Z1 JOB (640W1160100W116J0Z1,W100),'RTN W116S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*VCOM=&VCOM2                                                                  
//*COUNTRYX2=&COUNTRYX2                                                         
//*                                                                             
//********************************************************************          
//* VCOM - Supersession info full file to VIPS                                  
//********************************************************************          
//VCOM     EXEC W016P022,VCOM=&VCOM2                                            
//W01622.W016ZZD1 DD DSN=WUT.W116S1.W11649(+0),DISP=SHR                         
//*                                                                             
//********************************************************************          
//* MQ   - Supersession info full file to VIPS                                  
//********************************************************************          
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WUT.W116S1.W11649(+0)                                       
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=&COUNTRYX2                                                      
¤MQMPROP Vidb_Source=Full                                                       
¤MQMPROP LoadType=Full                                                          
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J0Z1                                         
