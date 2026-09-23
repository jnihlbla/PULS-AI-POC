//W612J309 JOB (640W6120100W612J309,W100),'RTN W612DD',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P009,                                                        
//        INDIN=&W612..W612DD,                                                  
//        INDUT=&W612..W612DD                                                   
//W61209.W61209D2 DD DUMMY                                                      
//W61209.W61209D3 DD DUMMY                                                      
//W61209.W61209D4 DD DUMMY                                                      
//W61209.W61209D5 DD DUMMY                                                      
//W61209.W61209D6 DD DUMMY                                                      
//W61209.W61209D7 DD DUMMY                                                      
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612DD.W61209(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612DD.W61209(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J309                                         
